/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.asm.optimiz;

import java.util.Iterator;
import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;

import scala.tools.asm.Opcodes;

import scala.tools.asm.Type;
import scala.tools.asm.tree.*;

import scala.tools.asm.tree.analysis.AnalyzerException;
import scala.tools.asm.tree.analysis.Frame;
import scala.tools.asm.tree.analysis.Value;
import scala.tools.asm.tree.analysis.SourceValue;

/**
 *  A class transformer focusing on private methods (including constructors), be they static or instance.
 *  Given that all possible callsites for the above can be found in the ClassNode given as argument to transform(),
 *  it's possible to:
 *    (a) determine for a private method or constructor which of its parameters see no use,
 *        removing them (which involves shifting usages of local vars and adapting the method descriptor),
 *    (b) adapt its invokers to drop the arguments for those params.
 *
 *  This transformation is useful as part of a larger code refactoring, rather than standalone.
 *
 *  As usual, care is required with code relying on private methods,
 *  including reflection or invokedynamic or a methodhandle constant.
 *  That's why rather than invoking transform() wholesale it's possible to selective apply
 *  this transform by invoking:
 *    (c) elideUnusedParams(ClassNode, MethodNode) followed by
 *    (d) elideArguments(cnode, caller, m, oldDescr, elidedParams)
 *
 *  Given this transformer inserts POP1 or POP2 instructions, afterwards PushPopCollapser should be run.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 */
public class UnusedParamsElider {

    public boolean changed;

    /**
     * @return those private MethodNodes whose method descriptor has changed (ie has fewer parameters).
     *         Please notice other MethodNodes may also have been adapted
     *         (in order to keep callsites to the above well-formed).
     * */
    public Set<MethodNode> transform(final ClassNode cnode) throws AnalyzerException {

        changed = false;

        Set<MethodNode> updatedMethodSignatures = new HashSet<MethodNode>();

        for (MethodNode m : cnode.methods) {
            if (Util.isPrivateMethod(m)) {
                String oldDescr = m.desc;
                Set<Integer> elidedParams = elideUnusedParams(cnode, m);
                if(!elidedParams.isEmpty()) {
                    changed = true;
                    updatedMethodSignatures.add(m);
                    for (MethodNode caller : cnode.methods) {
                        elideArguments(cnode, caller, cnode, m, oldDescr, elidedParams);
                    }
                }
            }
        }

        return updatedMethodSignatures;
    }

    /**
     *  Step 1 of 2 of the transformation:
     *    determine for a private method or constructor which of its parameters see no use,
     *    removing them (which involves shifting usages of local vars and adapting the method descriptor).
     *
     *  @param cnode class containing method m
     *  @param m     private method or constructors whose unused params are to be elided.
     *
     * */
    public static Set<Integer> elideUnusedParams(final ClassNode cnode, final MethodNode m) {
        assert Util.isPrivateMethod(m);
        assert cnode.methods.contains(m);

        // indexed by param position (which are zero-based)
        Type[] paramTs = Type.getArgumentTypes(m.desc);
        if(Util.isAbstractMethod(m) || paramTs.length == 0) {
            return java.util.Collections.EMPTY_SET;
        }

        if(m.maxLocals == 0) {
            Util.computeMaxLocalsMaxStack(m);
        }

        // indexed by local-var index, contains valid (positive) values for method params only.
        int[] originalSize     = new int[m.maxLocals];
        // indexed by local-var index, contains valid (zero-based) values for method params only.
        int[] originalParamPos = new int[m.maxLocals];
        // indexed by local-var index, contains valid (non-zero) values for method params only,
        // as follows: -1 stands for unused, while +1 stands for in-use.
        int[] paramStatus      = new int[m.maxLocals];

        final int firstParamLocalVarIdx = (Util.isInstanceMethod(m) ? 1 : 0);
        if(Util.isInstanceMethod(m)) {
            originalSize[0]       =  1;
            originalParamPos[0]   = -1; // -1 stands for the param position of the receiver for an instance method.
        }

        int idx = firstParamLocalVarIdx;
        for(int paramPos = 0; paramPos < paramTs.length; paramPos++) {
            int size = paramTs[paramPos].getSize();
            originalSize[idx]     = size;
            originalParamPos[idx] = paramPos;
            paramStatus[idx]      = -1; // stands for "unused param"
            idx += size;
        }

        // mark with +1 those params in use
        Iterator<AbstractInsnNode> insnIter = m.instructions.iterator();
        while(insnIter.hasNext()) {
            AbstractInsnNode insn = insnIter.next();
            if(insn.getOpcode() == Opcodes.RET) {
                return java.util.Collections.EMPTY_SET; // subroutines not supported
            }
            if(insn.getType() == AbstractInsnNode.VAR_INSN) {
                VarInsnNode vi = (VarInsnNode)insn;
                paramStatus[vi.var] = 1;
            }
        }

        Set<Integer> elidedParams = new HashSet<Integer>();

        /*
         * Visit starting from the last one each unused param, get its size,
         * decrease all usages with idx > param's idx (decrease by that size),
         * remove LocalVarNode if any.
         */
        for(idx = paramStatus.length - 1; idx >= firstParamLocalVarIdx; idx--) {
            if(paramStatus[idx] == -1) {

                int paramPos = originalParamPos[idx];
                paramTs[paramPos] = null;
                elidedParams.add(paramPos);

                insnIter = m.instructions.iterator();
                while(insnIter.hasNext()) {
                    AbstractInsnNode insn = insnIter.next();
                    if(insn.getType() == AbstractInsnNode.VAR_INSN) {
                        VarInsnNode vi = (VarInsnNode)insn;
                        assert(vi.var != idx); // we're assuming the param in question is NOT in use.
                        if(vi.var > idx) {
                            vi.var -= originalSize[idx];
                        }
                    }
                }

                Iterator<LocalVariableNode> lvnIter = m.localVariables.iterator();
                while(lvnIter.hasNext()) {
                    if(lvnIter.next().index == idx) {
                        lvnIter.remove();
                    }
                }
            }
        }

        if(!elidedParams.isEmpty()) {
            // gathering the types of non-elided params
            Type[] updatedParamTs = new Type[paramTs.length - elidedParams.size()];
            int updatedParamPos = 0;
            for(int oldParamPos = 0; oldParamPos < paramTs.length; oldParamPos++) {
                if(paramTs[oldParamPos] != null) {
                    updatedParamTs[updatedParamPos] = paramTs[oldParamPos];
                    updatedParamPos++;
                }
            }
            // updating the method descriptor
            Type updatedMethodType = Type.getMethodType(Type.getReturnType(m.desc), updatedParamTs);
            m.desc = updatedMethodType.getDescriptor();
            // updating maxLocals and maxStack
            Util.computeMaxLocalsMaxStack(m);
        }

        return elidedParams;

    }

    /**
     *  Step 2 of 2 of the transformation:
     *    adapt the invokers of a method some of whose params have been elided, dropping the arguments for those params.
     *
     *  @param callerOwner  class declaring method "caller"
     *  @param caller       a method in class cnode to be adapted in case it invokes the (already adapted) method callee.
     *  @param calleeOwner  class declaring method "callee"
     *  @param callee       (private) method whose unused params (given by elidedParams) have been elided.
     *  @param elidedParams zero-based positions of params that were elided in callee.
     *
     * */
    public static void elideArguments(final ClassNode    callerOwner,
                                      final MethodNode   caller,
                                      final ClassNode    calleeOwner,
                                      final MethodNode   callee,
                                      final String       oldDescr,
                                      final Set<Integer> elidedParams) throws AnalyzerException {

        assert callerOwner.methods.contains(caller);
        assert calleeOwner.methods.contains(callee);

        if(elidedParams.isEmpty() || Util.isAbstractMethod(caller)) {
            return;
        }

        // find callsites
        Set<MethodInsnNode> callsites = new HashSet<MethodInsnNode>();

        Iterator<AbstractInsnNode> insnIter = caller.instructions.iterator();
        while(insnIter.hasNext()) {
            AbstractInsnNode insn = insnIter.next();
            if(insn.getType() == AbstractInsnNode.METHOD_INSN) {
                MethodInsnNode mi = (MethodInsnNode)insn;
                if((mi.owner.equals(calleeOwner.name)) &&
                   (mi.name.equals(callee.name)) &&
                   (mi.desc.equals(oldDescr))) {

                    callsites.add(mi);
                }
            }
        }

        if(callsites.isEmpty()) {
            return;
        }

        // drop arguments for elided params
        dropArgumentsForElidedParams(callerOwner, caller, callsites, oldDescr, false, elidedParams);

        // update descriptor
        for(MethodInsnNode callsite : callsites) {
            callsite.desc = callee.desc;
        }

        // get rid of LOADs made redundant by the DROPs inserted, as well as dead-stores.
        UnusedParamsElider.elimRedundantLocalVarAccesses(callerOwner.name, caller);

        // not necessary to Util.computeMaxLocalsMaxStack(caller) because maxLocals was increased as need arose. maxStack doesn't change.
    }

    private boolean isThis(final int localVarIdx, final MethodNode m) {
        return (localVarIdx == 0) && Util.isInstanceMethod(m);
    }

    /**
     *  After refactoring a MethodNode by:
     *    (a) removing one or more method params, or
     *    (b) turning it into a static method,
     *  the need arises to adapt methods containing callsites so that the arguments in question (or the receiver)
     *  are dropped.
     *
     *  This method takes care of that, as needed by UnusedParamsElider and StaticsMaker.
     *
     *  The easiest way to "drop an unneeded argument" is by inserting a POP1 or POP2 instruction
     *  to remove the just pushed stack value. However that assumes no instructions other than the callsite being adapted
     *  consume the value in question (@see consumersExistOtherThanCallsite() ).
     *
     *  In case the unneded value has other consumers, the callsite in question has to be prefixed
     *  with STORE instructions that "spill" stack values into local-vars created to that effect,
     *  followed by LOAD instructions that load all spilled values except those that aren't needed
     *  (as a special case, removing the receiver via spilling amounts to POP1 right
     *  after all arguments values have been STOREd in local-vars).
     *
     *  @param cnode     class containing the method being adapted, caller
     *  @param caller    method containing callsites, each assuming more arguments (or a receiver) than
     *                   what the target method expects
     *  @param callsites callsites to be adapted (actually, the callsites themselves are left as is,
     *                   it's the surrounding code that is adapted).
     *  @param oldDescr  method descriptor for the target site, *before* method params were removed or
     *                   the method made static. Callsites still assume oldDescr.
     *  @param dropReceiver whether the receiver should be dropped. This can be combined with dropping zero or more
     *                      arguments, as given by elidedParams.
     *  @param elidedParams zero-based positions of arguments to drop, the receiver of an instance method
     *                      is not considered among these.
     *
     * */
    public static void dropArgumentsForElidedParams(final ClassNode     cnode,
                                                    final MethodNode    caller,
                                                    Set<MethodInsnNode> callsites,
                                                    String              oldDescr,
                                                    boolean             dropReceiver,
                                                    final Set<Integer>  elidedParams) throws AnalyzerException {

        assert cnode.methods.contains(caller);

        if(caller.maxLocals == 0) {
            Util.computeMaxLocalsMaxStack(caller);
        }

        ProdConsAnalyzer cp = ProdConsAnalyzer.create();
        cp.analyze(cnode.name, caller);

        Map<MethodInsnNode, Frame<SourceValue>> callsiteFrame = new HashMap<MethodInsnNode, Frame<SourceValue>>();

        Set<AbstractInsnNode> toPOP1 = new HashSet<AbstractInsnNode>();
        Set<AbstractInsnNode> toPOP2 = new HashSet<AbstractInsnNode>();
        Map<MethodInsnNode, Integer> toSpill = new HashMap<MethodInsnNode, Integer>();

        for(MethodInsnNode callsite : callsites) {
            assert caller.instructions.contains(callsite);
            Frame<SourceValue> frame = cp.frameAt(callsite);
            callsiteFrame.put(callsite, frame);

            boolean doneWithCallsite = false;
            if(dropReceiver) {
                SourceValue rcvProd = frame.getReceiver(callsite);
                if(consumersExistOtherThanCallsite(cp, rcvProd, callsite)) {
                    // drop unneeded value at the point of consumption
                    toSpill.put(callsite, -1);
                    doneWithCallsite = true;
                } else {
                    // drop unneeded value at the source
                    toPOP1.addAll(rcvProd.insns);
                }
            }

            Value[] argProducerss = frame.getActualArguments(callsite);
            for(int paramPos = 0; paramPos < argProducerss.length; paramPos++) {
                if(!doneWithCallsite && elidedParams.contains(paramPos)) {
                    SourceValue argProducers = (SourceValue)argProducerss[paramPos];
                    if(consumersExistOtherThanCallsite(cp, argProducers, callsite)) {
                        // drop unneeded value at the point of consumption
                        toSpill.put(callsite, paramPos);
                        doneWithCallsite = true;
                    } else {
                        // drop unneeded value at the source
                        if(argProducers.getSize() == 1) {
                            toPOP1.addAll(argProducers.insns);
                        } else {
                            toPOP2.addAll(argProducers.insns);
                        }
                    }
                }
            }
        }

        for(AbstractInsnNode prod1 : toPOP1) {
            caller.instructions.insert(prod1, Util.getDrop(1));
        }

        for(AbstractInsnNode prod2 : toPOP2) {
            caller.instructions.insert(prod2, Util.getDrop(2));
        }

        for(MethodInsnNode callsite : toSpill.keySet()) {
            Type[] argTs = Type.getArgumentTypes(oldDescr);
            boolean spillReceiver = false;
            int firstParamToDrop = toSpill.get(callsite);
            if(firstParamToDrop == -1) {
                spillReceiver = true;
                firstParamToDrop = 0;
            }
            assert spillReceiver || elidedParams.contains(firstParamToDrop);
            int[] idxOfAddedLocalVars = new int[argTs.length]; // indexed by paramPos
            for(int paramPos = argTs.length - 1; paramPos >= firstParamToDrop; paramPos--) {
                int idx = caller.maxLocals;
                idxOfAddedLocalVars[paramPos] = idx;
                caller.maxLocals += argTs[paramPos].getSize();
                VarInsnNode storeInsn = new VarInsnNode(argTs[paramPos].getOpcode(Opcodes.ISTORE), idx);
                caller.instructions.insertBefore(callsite, storeInsn);
            }
            if(spillReceiver) {
                caller.instructions.insertBefore(callsite, Util.getDrop(1));
            }
            for(int paramPos = firstParamToDrop; paramPos < argTs.length; paramPos++) {
                if(!elidedParams.contains(paramPos)) {
                    int idx = idxOfAddedLocalVars[paramPos];
                    VarInsnNode loadInsn = new VarInsnNode(argTs[paramPos].getOpcode(Opcodes.ILOAD), idx);
                    caller.instructions.insertBefore(callsite, loadInsn);
                }
            }
        }

    } // end of methdo dropArgumentsForElidedParams()

    private static boolean consumersExistOtherThanCallsite(ProdConsAnalyzer cp, SourceValue argProducers, MethodInsnNode callsite) {
        for(AbstractInsnNode prod : argProducers.insns) {
            if(!cp.hasUniqueImage(prod, callsite)) {
                return true;
            }
        }
        return false;
    }

    /**
     *  Get rid of LOADs made redundant by the DROPs inserted, as well as dead-stores.
     * */
    public static void elimRedundantLocalVarAccesses(final String cName, final MethodNode mnode) throws AnalyzerException {
      UnreachableCode  unreachableCode = new UnreachableCode();
      DeadStoreElim    deadStoreElim   = new DeadStoreElim();
      PushPopCollapser ppCollapser     = new PushPopCollapser();

      do {

          unreachableCode.transform(cName, mnode); // remove unreachable code
          deadStoreElim.transform(cName, mnode);   // replace STOREs to non-live local-vars with DROP instructions.
          ppCollapser.transform(cName, mnode);     // propagate a DROP to the instruction(s) that produce the value in question, drop the DROP.

      } while (deadStoreElim.changed || ppCollapser.changed);

    }
}
