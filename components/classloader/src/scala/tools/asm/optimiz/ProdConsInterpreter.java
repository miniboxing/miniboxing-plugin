/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.asm.optimiz;


import java.util.*;

import scala.tools.asm.Opcodes;
import scala.tools.asm.Type;
import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.FieldInsnNode;
import scala.tools.asm.tree.InvokeDynamicInsnNode;
import scala.tools.asm.tree.LdcInsnNode;
import scala.tools.asm.tree.MethodInsnNode;

import scala.tools.asm.tree.analysis.SourceInterpreter;
import scala.tools.asm.tree.analysis.SourceValue;
import scala.tools.asm.tree.analysis.AnalyzerException;

/**
 *  Given that a SourceInterpreter gets to see what instructions produce values for consumption by other instructions,
 *  we can piggyback on it to collect that information for later retrieval.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class ProdConsInterpreter extends SourceInterpreter {

    Map<AbstractInsnNode, Set<AbstractInsnNode>> prdcrs = new HashMap<AbstractInsnNode, Set<AbstractInsnNode>>();
    Map<AbstractInsnNode, Set<AbstractInsnNode>> cnsmrs = new HashMap<AbstractInsnNode, Set<AbstractInsnNode>>();

    public ProdConsInterpreter() {
        super(ASM4);
    }

    public ProdConsInterpreter(final int api) {
        super(api);
    }

    // ------------------------------------------------------------------------
    // utilities made available to clients
    // ------------------------------------------------------------------------

    /**
     *  returns the instructions that may produce the value(s) that the argument consumes.
     * */
    public Set<AbstractInsnNode> producers(final AbstractInsnNode insn) {
        Set<AbstractInsnNode> is = prdcrs.get(insn);
        return (is == null) ? Collections.<AbstractInsnNode>emptySet() : is;
    }

    /**
     *  returns the instructions that may consume the value(s) that the argument produces.
     * */
    public Set<AbstractInsnNode> consumers(final AbstractInsnNode insn) {
        Set<AbstractInsnNode> is = cnsmrs.get(insn);
        return (is == null) ? Collections.<AbstractInsnNode>emptySet() : is;
    }

    // ------------------------------------------------------------------------
    // internal methods
    // ------------------------------------------------------------------------

    protected void update(final SourceValue producers, final AbstractInsnNode consumer) {
        updateProducers(producers, consumer);
        updateConsumers(producers, consumer);
    }

    /** many producers, one consumer */
    private void updateProducers(final SourceValue producers, final AbstractInsnNode consumer) {
        Set<AbstractInsnNode> cluster = prdcrs.get(consumer);
        if(cluster == null) {
            cluster = new HashSet<AbstractInsnNode>();
            prdcrs.put(consumer, cluster);
        }
        cluster.addAll(producers.insns);
    }

    /** one consumer, many producers */
    private void updateConsumers(final SourceValue producers, final AbstractInsnNode consumer) {
        Iterator<AbstractInsnNode> iter = producers.insns.iterator();
        while(iter.hasNext()) {
            AbstractInsnNode producer = iter.next();
            Set<AbstractInsnNode> cluster = cnsmrs.get(producer);
            if(cluster == null) {
                cluster = new HashSet<AbstractInsnNode>();
                cnsmrs.put(producer, cluster);
            }
            cluster.add(consumer);
        }
    }

    @Override
    public SourceValue copyOperation(final AbstractInsnNode insn, final SourceValue value) {
        update(value, insn);
        return super.copyOperation(insn, value);
    }

    @Override
    public SourceValue unaryOperation(final AbstractInsnNode insn, final SourceValue value) {
        update(value, insn);
        return super.unaryOperation(insn, value);
    }

    @Override
    public SourceValue binaryOperation(
        final AbstractInsnNode insn,
        final SourceValue value1,
        final SourceValue value2)
    {
        update(value1, insn);
        update(value2, insn);
        return super.binaryOperation(insn, value1, value2);
    }

    @Override
    public SourceValue ternaryOperation(
        final AbstractInsnNode insn,
        final SourceValue value1,
        final SourceValue value2,
        final SourceValue value3)
    {
        update(value1, insn);
        update(value2, insn);
        update(value3, insn);
        return super.ternaryOperation(insn, value1, value2, value3);
    }

    @Override
    public SourceValue naryOperation(final AbstractInsnNode insn, final List<? extends SourceValue> values) {
        Iterator<? extends SourceValue> vIter = values.iterator();
        while (vIter.hasNext()) {
            update(vIter.next(), insn);
        }
        return super.naryOperation(insn, values);
    }

    @Override
    public void returnOperation(
        final AbstractInsnNode insn,
        final SourceValue value,
        final SourceValue expected)
    {
        update(value, insn);
        // `expected` does not denote a producer instruction.
        super.returnOperation(insn, value, expected);
    }

    @Override
    public void drop(
        AbstractInsnNode insn,
        final SourceValue value) throws AnalyzerException
    {
        update(value, insn);
        super.drop(insn, value);
    }

}
