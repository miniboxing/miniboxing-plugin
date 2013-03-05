/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.asm.optimiz;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Queue;
import java.util.LinkedList;

import scala.tools.asm.Opcodes;
import scala.tools.asm.Type;
import scala.tools.asm.tree.MethodNode;
import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.VarInsnNode;
import scala.tools.asm.tree.MethodInsnNode;

import scala.tools.asm.tree.analysis.AnalyzerException;
import scala.tools.asm.tree.analysis.Frame;
import scala.tools.asm.tree.analysis.SourceValue;

/**
 *  This method transformer recognizes the following code pattern:
 *
 *       box-1  box-2  ... box-N
 *          \      |      /
 *           \     |     /
 *            \    |    /
 *             \   |   /
 *              \  |  /
 *               \ | /
 *              phi node
 *               / | \
 *              /  |  \
 *             /   |   \
 *            /    |    \
 *           /     |     \
 *          /      |      \
 *     unbox-1  unbox-2 ... unbox-M
 *
 *  And proceeds to remove all of the unbox and box operations.
 *
 *  The preconditions for activating this rewriting are:
 *
 *    (a) the only producers of values in the web are box   operations (be they Java or Scala boxing, see `isBox()`)
 *
 *    (b) the only consumers of values in the web are unbox operations (be they Java or Scala boxing, see `isUnBox()`)
 *
 *    (c) N > 0 and M > 0. It's not necessary for M == N.
 *
 *  Implementation notes:
 *
 *    Precondition (a) guarantees the values reaching the unbox operations are non-null,
 *    which makes a difference for Java unbox (Scala unboxing simply converts null to zero).
 *    "Makes a difference" as in "side-effects" (Java unbox NPEs on null).
 *    Alternatively, a nullness-analysis could be used, and in case it guaranteed
 *    all producers deliver non-null, they would be fine. But this transformation doesn't track non-nullness,
 *    thus the precondition above.
 *
 *    What is depicted as "phi node" in the diagram can be a combination of copies between stack slots and loca vars.
 *    This motivates turning those ALOAD, ASTORE instructions into their counterpart for a primitive (unboxed) value.
 *    In particular, when the primitive is category-2 (J or D) then two local-slots are needed.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class UnBoxElider {

    /** after transform() has run, this field records whether
     *    at least one pass of this transformer modified something. */
    public boolean changed = false;

    private String owner = null;
    private MethodNode mnode = null;

    public void transform(final String owner, final MethodNode mnode) throws AnalyzerException {

        changed = false;
        this.owner = owner;
        this.mnode = mnode;

        // compute the produce-consume relation (ie values flow from "producer" instructions to "consumer" instructions).
        UnBoxAnalyzer cp = UnBoxAnalyzer.create();
        cp.analyze(owner, mnode);

        Frame<SourceValue>[] frames = cp.getFrames();
        AbstractInsnNode[]   insns  = mnode.instructions.toArray();

        Set<AbstractInsnNode> skipExam = new HashSet<AbstractInsnNode>();

        int i = 0;
        while(i < insns.length) {

            if (frames[i] != null && insns[i] != null) {

                if(isUnBox(insns[i]) && !skipExam.contains(insns[i])) {

                    MethodInsnNode ubox = (MethodInsnNode)insns[i];

                    // test of necessary condition, saves testing full condition most of the time
                    Set<AbstractInsnNode> directProducers = cp.producers(ubox);
                    if(areAllBox(directProducers)) {

                        Set<AbstractInsnNode> allProducers = new HashSet<AbstractInsnNode>();
                        Set<AbstractInsnNode> allConsumers = new HashSet<AbstractInsnNode>();

                        cp.fixpointGivenConsumer(ubox, allProducers, allConsumers);

                        assert !allConsumers.isEmpty();
                        assert !allProducers.isEmpty();

                        if(areAllUnBox(allConsumers) && areAllBox(allProducers) && samePrimitive(allProducers, allConsumers) &&
                           areDisjoint(allProducers, skipExam) && areDisjoint(allConsumers, skipExam)) {

                            // those local var used to convey boxed values will now convey unboxed ones
                            // ie ALOAD has to be converted to (I | F | L | D)LOAD, similarly for local-var store instructions.
                            updateCopyOps(allProducers, allConsumers, Type.getReturnType(ubox.desc));

                            skipExam.addAll(allProducers);
                            skipExam.addAll(allConsumers);

                            // the actual transform: cancel out all boxes and all unboxes by removing them.
                            removeAll(allProducers);
                            removeAll(allConsumers);

                            changed = true;

                        }

                    }

                }

            }
            i += 1;
        }

    }

    private void updateCopyOps(Set<AbstractInsnNode> producers, Set<AbstractInsnNode> consumers, Type retType) throws AnalyzerException {

        final Set<AbstractInsnNode>   visited  = new HashSet<AbstractInsnNode>();
        final Set<AbstractInsnNode>   visiting = new HashSet<AbstractInsnNode>();
        final Queue<AbstractInsnNode> toVisit  = new LinkedList<AbstractInsnNode>();

        visiting.addAll(producers);

        final ProdConsAnalyzer a = ProdConsAnalyzer.create();
        a.analyze(owner, mnode);

        Map<Integer, Integer> widenedVars = new HashMap<Integer, Integer>();

        do {

            final Iterator<AbstractInsnNode> iterV = visiting.iterator();
            while(iterV.hasNext()) {
                final Set<AbstractInsnNode> cs = a.consumers(iterV.next());
                final Iterator<AbstractInsnNode> iterC = cs.iterator();
                while(iterC.hasNext()) {
                    final AbstractInsnNode insn = iterC.next();
                    if(!consumers.contains(insn) && !visited.contains(insn)) {
                        final int opc = insn.getOpcode();
                        assert (opc == Opcodes.ASTORE) || (opc == Opcodes.ALOAD);
                        visited.add(insn);

                        final int originalVarIdx = ((VarInsnNode)insn).var;
                        int updVarIdx = originalVarIdx;
                        if(retType.getSize() == 2) {
                            if(!widenedVars.containsKey(originalVarIdx)) {
                                widenedVars.put(originalVarIdx, mnode.maxLocals);
                                mnode.maxLocals += 2;
                            }
                            updVarIdx = widenedVars.get(originalVarIdx);
                        }

                        AbstractInsnNode updInsn =
                                new VarInsnNode(
                                        retType.getOpcode( (opc == Opcodes.ASTORE) ? Opcodes.ISTORE : Opcodes.ILOAD ),
                                        updVarIdx
                                );
                        mnode.instructions.set(insn, updInsn);
                        toVisit.add(insn);
                    }
                }
            }
            visiting.clear();
            visiting.addAll(toVisit);
            toVisit.clear();

        } while(!visiting.isEmpty());

    }

    private static boolean isFake(final AbstractInsnNode insn) {
        return (insn instanceof UnBoxAnalyzer.FakeInsn);
    }

    private static boolean isUnBox(final AbstractInsnNode insn) {
        return !isFake(insn) && (SSLUtil.isScalaUnBox(insn) || Util.isJavaUnBox(insn));
    }

    private static boolean isBox(final AbstractInsnNode insn) {
        return !isFake(insn) && (SSLUtil.isScalaBox(insn) || Util.isJavaBox(insn));
    }

    private void removeAll(Set<AbstractInsnNode> is) {
        Iterator<AbstractInsnNode> iter = is.iterator();
        while(iter.hasNext()) {
            mnode.instructions.remove(iter.next());
        }
    }

    private boolean areAllUnBox(Set<AbstractInsnNode> is) {
        Iterator<AbstractInsnNode> iter = is.iterator();
        while(iter.hasNext()) {
            if(!isUnBox(iter.next())) {
                return false;
            }
        }
        return true;
    }

    private boolean areAllBox(Set<AbstractInsnNode> is) {
        Iterator<AbstractInsnNode> iter = is.iterator();
        while(iter.hasNext()) {
            if(!isBox(iter.next())) {
                return false;
            }
        }
        return true;
    }

    private boolean areDisjoint(Set<AbstractInsnNode> a, Set<AbstractInsnNode> b) {
        Iterator<AbstractInsnNode> iter = a.iterator();
        while(iter.hasNext()) {
            if(b.contains(iter.next())) {
                return false;
            }
        }
        return true;
    }

    // SI-6547
    private boolean samePrimitive(final Set<AbstractInsnNode> producers, final Set<AbstractInsnNode> consumers) {
        // question: do all producers box from the same primtive?
        final Iterator<AbstractInsnNode> iterProds = producers.iterator();
        final Type bt = boxArgType(iterProds.next());
        while(iterProds.hasNext()) {
            final boolean sameAsPrev = bt.equals(boxArgType(iterProds.next()));
            if(!sameAsPrev) {
                return false;
            }
        }

        // question: do all consumers unbox to the same primtive?
        final Iterator<AbstractInsnNode> iterCons = consumers.iterator();
        final Type ut = unboxRetType(iterCons.next());
        while(iterCons.hasNext()) {
            final boolean sameAsPrev = ut.equals(unboxRetType(iterCons.next()));
            if(!sameAsPrev) {
                return false;
            }
        }

        // question: is it the same primtive that's being boxed from and unboxed to?
        return bt.equals(ut);
    }

    private Type unboxRetType(final AbstractInsnNode unboxCallsite) {
        final MethodInsnNode callsite = (MethodInsnNode)unboxCallsite;
        return Type.getReturnType(callsite.desc);
    }

    private Type boxArgType(final AbstractInsnNode boxCallsite) {
        final MethodInsnNode callsite = (MethodInsnNode)boxCallsite;
        return Type.getArgumentTypes(callsite.desc)[0];
    }

}

