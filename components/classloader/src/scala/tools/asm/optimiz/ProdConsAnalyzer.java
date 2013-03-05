/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.asm.optimiz;

import java.util.Set;
import java.util.Iterator;
import java.util.Queue;
import java.util.LinkedList;
import java.util.HashSet;

import scala.tools.asm.tree.*;

import scala.tools.asm.tree.analysis.Analyzer;
import scala.tools.asm.tree.analysis.AnalyzerException;
import scala.tools.asm.tree.analysis.SourceValue;
import scala.tools.asm.tree.analysis.Frame;
/**
 *  A SourceInterpreter can be used in conjunction with an Analyzer
 *  to compute, for each instruction, a Frame containing for each location P
 *  (local-var or stack-slot) the instructions that may produce the value in P.
 *
 *  Oftentimes, we want to invert that map
 *  ie we want to find all the possible consumers of values that a given instruction produces.
 *
 *  After `analyze()` has run:
 *    - consumers(insn) returns the set of instructions that may consume at least one value produced by `insn`.
 *                      "at least" because DUP produces two values.
 *    - producers(insn) returns the set of instructions that may produce at least one value consumed by `insn`.
 *
 *  Alternative terminology:
 *     - those definitions reaching insn are given by `producers(insn)`
 *
 *  This in turn allows computing:
 *      - du-chains (definition-use chains)
 *      - ud-chains (use-definition chains)
 *      - webs
 *      as covered in Sec. 8.10 of
 *        Steven S. Muchnick. Advanced compiler design and implementation.
 *        Morgan Kaufmann Publishers Inc., San Francisco, CA, USA, 1997.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class ProdConsAnalyzer extends Analyzer<SourceValue> {

    public static ProdConsAnalyzer create() {
        return new ProdConsAnalyzer(new ProdConsInterpreter());
    }

    private final ProdConsInterpreter pt;

    public ProdConsAnalyzer(ProdConsInterpreter pt) {
        super(pt);
        this.pt = pt;
    }

    MethodNode mnode = null;
    Frame<SourceValue>[] frames = null;

    // ------------------------------------------------------------------------
    // utilities made available to clients
    // ------------------------------------------------------------------------

    public Frame<SourceValue>[] analyze(final String owner, final MethodNode mnode) throws AnalyzerException {
        this.mnode = mnode;
        frames = super.analyze(owner, mnode);
        return frames;
    }

    @Override
    public Frame<SourceValue> frameAt(final AbstractInsnNode insn) {
        int idx = mnode.instructions.indexOf(insn);
        return frames[idx];
    }

    /**
     *  returns the instructions that may produce the value(s) that the argument consumes.
     * */
    public Set<AbstractInsnNode> producers(final AbstractInsnNode insn) {
        return pt.producers(insn);
    }

    /**
     *  returns the instructions that may consume the value(s) that the argument produces.
     * */
    public Set<AbstractInsnNode> consumers(final AbstractInsnNode insn) {
        return pt.consumers(insn);
    }

    /**
     *  returns the instructions that may consume the value(s) that the local-var given by `idx` gets throughout the method.
     * */
    public Set<AbstractInsnNode> consumersOfLocalVar(final int idx) {
        Set<AbstractInsnNode> result = new HashSet<AbstractInsnNode>();
        Iterator<AbstractInsnNode> iter = mnode.instructions.iterator();
        while (iter.hasNext()) {
            AbstractInsnNode insn = iter.next();
            if(Util.isLOAD(insn)) {
                VarInsnNode v = (VarInsnNode)insn;
                if(v.var == idx) {
                    result.addAll(consumers(v));
                }
            }
        }
        return result;
    }

    /**
     *  This method checks whether a multiplexer connects producers to consumer,
     *  ie whether `consumer` is the only concentrator for values `producers` deliver.
     *
     *  This method returns true iff
     *    (a) each value one of the `producers` delivers has `consumer` as single recipient; and
     *    (b) `consumer` may receive values from any instruction listed in `producers` and no others.
     *
     */
    public boolean isMux(final Set<AbstractInsnNode> producers, final AbstractInsnNode consumer) {
        assert !producers.isEmpty();
        // condition (b)
        if(!producers(consumer).equals(producers)) {
            return false;
        }
        // condition (a)
        Iterator<AbstractInsnNode> iter = producers.iterator();
        while (iter.hasNext()) {
            AbstractInsnNode prod = iter.next();
            if(!hasUniqueImage(prod, consumer)) {
                return false;
            }
        }
        return true;
    }

    /**
     *  This method checks whether a demultiplexer connects producer to consumers,
     *  ie whether `producer` is an spreader of values that any sink in `consumers` may receive.
     *
     *  This method returns true iff
     *    (a) each value `producer` delivers must end up in one of the `consumers`; and
     *    (b) each of the `consumers` may receive a value from the `producer` and no other.
     *
     */
    public boolean isDemux(final AbstractInsnNode producer, final Set<AbstractInsnNode> consumers) {
        assert !consumers.isEmpty();
        // condition (a)
        if(!consumers(producer).equals(consumers)) {
            return false;
        }
        // condition (b)
        Iterator<AbstractInsnNode> iter = consumers.iterator();
        while (iter.hasNext()) {
            AbstractInsnNode cons = iter.next();
            if(!hasUniquePreimage(producer, cons)) {
                return false;
            }
        }
        return true;
    }

    /**
     *  Does the value `producer` deliver invariably end up in `consumer`? (ie no other recipient is possible)
     *  Please notice `consumer` may receive values from sources other than `producer`.
     */
    public boolean hasUniqueImage(final AbstractInsnNode producer, final AbstractInsnNode consumer) {
        return isSingleton(consumers(producer), consumer);
    }

    /**
     *  Does the value `consumer` receives invariably originate in `producer`? (ie no other source is possible)
     *  Please notice `producer` may deliver values to sinks other than `consumer`.
     */
    public boolean hasUniquePreimage(final AbstractInsnNode producer, final AbstractInsnNode consumer) {
        return isSingleton(producers(consumer), producer);
    }

    /**
     *  Does the value `consumer` receives invariably originate in `producer`, and moreover
     *  is `consumer` the only sink for `producer`?
     */
    public boolean isPointToPoint(final AbstractInsnNode producer, final AbstractInsnNode consumer) {
        return hasUniqueImage(producer, consumer) && hasUniquePreimage(producer, consumer);
    }

    /**
     *  Is `insns` a singleton set whose only element is given by `elem`?
     *  (object identity test between `elem` and the singleton's element)
     */
    public boolean isSingleton(final Set<AbstractInsnNode> insns, final AbstractInsnNode elem) {
        return (insns.size() == 1) && (insns.iterator().next() == elem);
    }

    /**
     *  @return In case `insns` is a singleton set, its only element. Null otherwise.
     */
    public AbstractInsnNode getSingleton(final Set<AbstractInsnNode> insns) {
        if(insns.size() == 1) {
            return insns.iterator().next();
        } else {
            return null;
        }
    }

        // ------------------------------------------------------------------------
        // functionality used by UnBoxElider
        // ------------------------------------------------------------------------

    /**
     *  Given a `consumer` instruction, finds all its producers, for each all its consumers, for each all its producers,
     *  and so on until a fixpoint is reached. End results are returned in `allProducers` and `allConsumers`.
     *
     * */
    public void fixpointGivenConsumer(final AbstractInsnNode consumer, final Set<AbstractInsnNode> allProducers, final Set<AbstractInsnNode> allConsumers) {

        Queue<AbstractInsnNode> cq = new LinkedList<AbstractInsnNode>();
        Queue<AbstractInsnNode> pq = new LinkedList<AbstractInsnNode>();

        cq.add(consumer);

        do {

            Iterator<AbstractInsnNode> iterProd = pq.iterator();
            while (iterProd.hasNext()) {
                AbstractInsnNode p = iterProd.next();
                allProducers.add(p);
                Set<AbstractInsnNode> cs = consumers(p);
                Iterator<AbstractInsnNode> iterCons = cs.iterator();
                while(iterCons.hasNext()) {
                    AbstractInsnNode c = iterCons.next();
                    if(!allConsumers.contains(c)) {
                        cq.add(c);
                    }
                }
            }
            pq.clear();

            Iterator<AbstractInsnNode> iterCons = cq.iterator();
            while (iterCons.hasNext()) {
                AbstractInsnNode c = iterCons.next();
                allConsumers.add(c);
                Set<AbstractInsnNode> ps = producers(c);
                iterProd = ps.iterator();
                while(iterProd.hasNext()) {
                    AbstractInsnNode p = iterProd.next();
                    if(!allProducers.contains(p)) {
                        pq.add(p);
                    }
                }
            }
            cq.clear();


        } while(!pq.isEmpty() || !cq.isEmpty());

    }

    public void fixpointGivenProducer(final AbstractInsnNode producer, final Set<AbstractInsnNode> allProducers, final Set<AbstractInsnNode> allConsumers) {
        AbstractInsnNode bootstrapConsumer = consumers(producer).iterator().next();
        fixpointGivenConsumer(bootstrapConsumer, allProducers, allConsumers);
    }

    // ------------------------------------------------------------------------
    // internal methods
    // ------------------------------------------------------------------------

}
