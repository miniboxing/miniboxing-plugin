/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.asm.optimiz;

import java.util.List;

import scala.tools.asm.Opcodes;
import scala.tools.asm.Type;

import scala.tools.asm.tree.MethodNode;
import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.VarInsnNode;
import scala.tools.asm.tree.LookupSwitchInsnNode;
import scala.tools.asm.tree.JumpInsnNode;
import scala.tools.asm.tree.LabelNode;
import scala.tools.asm.tree.InsnNode;
import scala.tools.asm.tree.TableSwitchInsnNode;
import scala.tools.asm.tree.InsnList;
import scala.tools.asm.tree.IntInsnNode;
import scala.tools.asm.tree.LdcInsnNode;
import scala.tools.asm.tree.IincInsnNode;

import scala.tools.asm.tree.analysis.AnalyzerException;
import scala.tools.asm.tree.analysis.Analyzer;
import scala.tools.asm.tree.analysis.Frame;
import scala.tools.asm.tree.analysis.Value;
import scala.tools.asm.tree.analysis.Interpreter;

/**
 *  This method transformer works in three steps:
 *
 *    (1) a pre-state is computed for each instruction (using ConstantInterpreter)
 *        classifying values into Unknown or Constant. The latter denote primitive constants only (for nullness propagation use NullnessPropagator).
 *
 *    (2) a single pass is made over the method's instructions. This is where constants are "propagated".
 *
 *          (a) loads that can be expressed as shorthand (e.g. ICONST_1)
 *
 *          (b) binary operations with constant result are left in place,
 *              but a DROP and a load of the constant-result are inserted rigth after them.
 *              That way, the next round of PushPopCollapser will back-propagate the DROP to the operation's arguments,
 *              leaving just the constant-load in place.
 *
 *          (c) conditional jumps, as well as lookup and table switches are simplified when their operand is constant.
 *              Details of the simplification differ only in the way the jump-destination is determined:
 *
 *                - conditional jumps (ie if<cond> and if_icmp<cond> instructions)
 *                  are removed, leaving in their place a DROP and an unconditional jump (again, PushPopCollapser will backpropagate the DROP)
 *
 *                - after looking up the target, a tableswitch or lookupswith instruction is removed,
 *                  leaving in its place a DROP and an unconditional jump (PushPopCollapser will simplify further).
 *
 *                The reason for leaving work for PushPopCollapser to do is that it contains the logic to preserve side-effects,
 *                and eliding the rest. We reuse all that at the cost of an additional dataflow iteration.
 *
 *    (3) unreachable code elimination, to remove from the instruction stream what (2.c) above has made dead code.
 *
 *  The ConstantInterpreter starts off with Unknown values for params and for exceptions in EHs, see `ConstantInterpreter.newValue()`
 *  The only place where Constant values are introduced is via loads (ICONST_1 etc).
 *
 *  A more general approach is covered in:
 *      Durica Nikolic, Fausto Spoto: Definite Expression Aliasing Analysis for Java Bytecode. ICTAC 2012: 74-89
 *      http://profs.sci.univr.it/~nikolic/download/ICTAC2012/ICTAC2012Ext.pdf
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 */
public class ConstantFolder implements Opcodes {

    /** after transform() has run, this field records whether
     *  at least one pass of this transformer modified something. */
    public boolean changed = false;

    private MethodNode mnode = null;

    private UnreachableCode unreachCodeRemover = new UnreachableCode();

    public void transform(final String owner, final MethodNode mnode) throws AnalyzerException {

        this.mnode = mnode;

        Analyzer<CFValue> propag = new Analyzer<CFValue>(new ConstantInterpreter());
        propag.analyze(owner, mnode);

        Frame<CFValue>[] frames = propag.getFrames();
        AbstractInsnNode[] insns = mnode.instructions.toArray();

        changed = false;

        int i = 0;
        while(i < insns.length) {

            Frame<CFValue>   frame    = frames[i];
            Frame<CFValue>   nxtFrame = (i+1 < frames.length) ? frames[i+1] : null;
            AbstractInsnNode insn  = insns[i];

            CFValue value1   = null;
            CFValue value2   = null;
            boolean succeeds = false;

            if(insn != null) {

                int opc = insn.getOpcode();
                switch (opc) {

                    case Opcodes.ILOAD:
                    case Opcodes.LLOAD:
                    case Opcodes.FLOAD:
                    case Opcodes.DLOAD:
                        VarInsnNode vin = (VarInsnNode)insn;
                        CFValue vv = frame.getLocal(vin.var);
                        if(vv.isConstant()) {
                            AbstractInsnNode lin = ((Constant)vv).pushInsn();
                            if(lin.getType() == AbstractInsnNode.INSN) {
                                // InsNode instructions takes just one byte
                                mnode.instructions.set(insn, lin);
                                changed = true; // actually not needed: control-flow unaltered, no code has been killed.
                            }
                        }
                        break;

                    case Opcodes.IADD:
                    case Opcodes.LADD:
                    case Opcodes.FADD:
                    case Opcodes.DADD:
                    case Opcodes.ISUB:
                    case Opcodes.LSUB:
                    case Opcodes.FSUB:
                    case Opcodes.DSUB:
                    case Opcodes.IMUL:
                    case Opcodes.LMUL:
                    case Opcodes.FMUL:
                    case Opcodes.DMUL:
                    case Opcodes.IDIV:
                    case Opcodes.LDIV:
                    case Opcodes.FDIV:
                    case Opcodes.DDIV:

                    case Opcodes.INEG:
                    case Opcodes.LNEG:
                    case Opcodes.FNEG:
                    case Opcodes.DNEG:

                    case Opcodes.IAND:
                    case Opcodes.LAND:
                    case Opcodes.IOR:
                    case Opcodes.LOR:

                    case Opcodes.I2L:
                    case Opcodes.I2F:
                    case Opcodes.I2D:
                    case Opcodes.L2I:
                    case Opcodes.L2F:
                    case Opcodes.L2D:
                    case Opcodes.F2I:
                    case Opcodes.F2L:
                    case Opcodes.F2D:
                    case Opcodes.D2I:
                    case Opcodes.D2L:
                    case Opcodes.D2F:
                    case Opcodes.I2B:
                    case Opcodes.I2C:
                    case Opcodes.I2S:

                    case Opcodes.LCMP:

                        if(nxtFrame.getStackTop().isConstant()) {
                            dropAndLoad(insn, (Constant)nxtFrame.getStackTop());
                            changed = true; // actually not needed: control-flow unaltered, no code has been killed.
                        }
                        break;

                    case Opcodes.IFEQ:
                    case Opcodes.IFNE:
                    case Opcodes.IFLT:
                    case Opcodes.IFGE:
                    case Opcodes.IFGT:
                    case Opcodes.IFLE:
                        if(frame.getStackTop().isConstant()) {
                            ICst ic = (ICst) frame.getStackTop();
                            switch (opc) {
                                case Opcodes.IFEQ: succeeds = ic.v == 0; break;
                                case Opcodes.IFNE: succeeds = ic.v != 0; break;
                                case Opcodes.IFLT: succeeds = ic.v  < 0; break;
                                case Opcodes.IFGE: succeeds = ic.v >= 0; break;
                                case Opcodes.IFGT: succeeds = ic.v  > 0; break;
                                case Opcodes.IFLE: succeeds = ic.v <= 0; break;
                            }
                            InsnList is = dropAndJump(1, succeeds, ((JumpInsnNode) insn).label);
                            mnode.instructions.insert(insn, is);
                            mnode.instructions.remove(insn);
                            changed = true;
                        }
                        break;

                    case Opcodes.IF_ICMPEQ:
                    case Opcodes.IF_ICMPNE:
                    case Opcodes.IF_ICMPLT:
                    case Opcodes.IF_ICMPGE:
                    case Opcodes.IF_ICMPGT:
                    case Opcodes.IF_ICMPLE:
                        value2 = frame.getStackTop();
                        value1 = frame.peekDown(1);
                        if(value1.isConstant() && value2.isConstant()) {
                            ICst i1 = (ICst) value1;
                            ICst i2 = (ICst) value2;
                            switch (opc) {
                                case Opcodes.IF_ICMPEQ: succeeds = i1.v == i2.v; break;
                                case Opcodes.IF_ICMPNE: succeeds = i1.v != i2.v; break;
                                case Opcodes.IF_ICMPLT: succeeds = i1.v  < i2.v; break;
                                case Opcodes.IF_ICMPGE: succeeds = i1.v >= i2.v; break;
                                case Opcodes.IF_ICMPGT: succeeds = i1.v  > i2.v; break;
                                case Opcodes.IF_ICMPLE: succeeds = i1.v <= i2.v; break;
                            }
                            InsnList is = dropAndJump(2, succeeds, ((JumpInsnNode) insn).label);
                            mnode.instructions.insert(insn, is);
                            mnode.instructions.remove(insn);
                            changed = true;
                        }
                        break;

                    case Opcodes.TABLESWITCH:
                    case Opcodes.LOOKUPSWITCH:
                        if(frame.getStackTop().isConstant()) {
                            ICst ic = (ICst) frame.getStackTop();
                            LabelNode dest = null;
                            switch (opc) {
                                case Opcodes.TABLESWITCH:
                                    TableSwitchInsnNode tsin = (TableSwitchInsnNode)insn;
                                    if(ic.v < tsin.min || ic.v > tsin.max) {
                                        dest = tsin.dflt;
                                    } else {
                                        dest = tsin.labels.get(ic.v - tsin.min);
                                    }
                                    break;
                                case Opcodes.LOOKUPSWITCH:
                                    LookupSwitchInsnNode lsin = (LookupSwitchInsnNode)insn;
                                    int keyIdx = lsin.keys.indexOf(ic.v);
                                    dest = (keyIdx == -1) ? lsin.dflt : lsin.labels.get(keyIdx);
                                    break;
                            }
                            InsnList is = dropAndJump(1, true, dest);
                            mnode.instructions.insert(insn, is);
                            mnode.instructions.remove(insn);
                            changed = true;
                        }
                        break;

                    default:
                        ;

                }

            }

            i += 1;
        }

        if(changed) {
            // UnreachableCode eliminates null frames (which complicate further analyses).
            unreachCodeRemover.transform(owner, mnode);
        }

    }

    private InsnList dropAndJump(final int drops, final boolean takeTheJump, LabelNode label) {
        assert drops == 1 || drops == 2;
        InsnNode d  = new InsnNode(Opcodes.POP);
        InsnList is = new InsnList();
        is.add(d);
        if(drops == 2) {
           is.add(new InsnNode(Opcodes.POP));
        }
        if(takeTheJump) {
            is.add(new JumpInsnNode(Opcodes.GOTO, label));
        }
        return is;
    }

    private void dropAndLoad(final AbstractInsnNode insn, Constant cst) {
        mnode.instructions.insert(insn, cst.pushInsn());
        // PushPopCollapser will back-propagate the following drop instruction.
        mnode.instructions.insert(insn, Util.getDrop(cst.getSize()));
    }

    static private abstract class CFValue implements Value {

        protected final int size;

        public CFValue(int size) {
            this.size = size;
        }

        @Override
        final public int getSize() { return size; }

        public boolean isConstant() { return false; }

        final public boolean isUnknown() { return !isConstant(); } // we have just one class of "unknowns" (as opposed to e.g. LessThanZero, etc.)

        public abstract CFValue convI();
        public abstract CFValue convF();
        public abstract CFValue convJ();
        public abstract CFValue convD();

        public abstract CFValue iinc(int incr);
        public abstract CFValue neg();

        public abstract CFValue add(CFValue value2);
        public abstract CFValue sub(CFValue value2);
        public abstract CFValue mul(CFValue value2);
        public abstract CFValue div(CFValue value2);

        public abstract CFValue and(CFValue value2);
        public abstract CFValue or (CFValue value2);

        public abstract boolean equals(final Object value);
        public abstract int hashCode();

    } // end of nested class CFValue

    final static private class Unknown extends CFValue {

        static public Unknown UNKNOWN_1 = new Unknown(1);

        static public Unknown UNKNOWN_2 = new Unknown(2);

        public Unknown(int size) {
            super(size);
        }

        @Override public Unknown convI() { return Unknown.UNKNOWN_1; }
        @Override public Unknown convF() { return Unknown.UNKNOWN_1; }
        @Override public Unknown convJ() { return Unknown.UNKNOWN_2; }
        @Override public Unknown convD() { return Unknown.UNKNOWN_2; }

        @Override public Unknown iinc(int incr) { return this; }
        @Override public Unknown neg()  { return this; }

        @Override public Unknown add(CFValue value2) { assert size == value2.size; return this; }
        @Override public Unknown sub(CFValue value2) { assert size == value2.size; return this; }

        @Override public CFValue mul(CFValue value2) {
            assert size == value2.size;
            if(value2.isConstant()) {
                final Constant cst = (Constant)value2;
                if(cst.isIntegral()) {
                    boolean isZero;
                    if(cst.sort == Type.LONG) {
                        JCst jc = (JCst) cst;
                        isZero = (jc.v == 0);
                    } else {
                        ICst ic = (ICst) cst;
                        isZero = (ic.v == 0);
                    }
                    if(isZero) {
                        return value2;
                    }
                }
            }
            return this;
        }

        @Override public Unknown div(CFValue value2) { assert size == value2.size; return this; }

        @Override public CFValue and(CFValue value2) {
            assert size == value2.size;
            CFValue result = this;
            if(value2.isConstant()) {
                if(value2 instanceof ICst) {
                    ICst that = (ICst)value2;
                    if(that.v == 0) {
                        result = that;
                    }
                } else if(value2 instanceof JCst) {
                    JCst that = (JCst)value2;
                    if(that.v == 0L) {
                        result = that;
                    }
                }
            }
            return result;
        }
        @Override public CFValue or (CFValue value2) {
            assert size == value2.size;
            CFValue result = this;
            if(value2.isConstant()) {
                if(value2 instanceof ICst) {
                    ICst that = (ICst)value2;
                    if(that.v == 0xFFFFFFFF) {
                        result = that;
                    }
                } else if(value2 instanceof JCst) {
                    JCst that = (JCst)value2;
                    if(that.v == 0xFFFFFFFFFFFFFFFFL) {
                        result = that;
                    }
                }
            }
            return result;
        }

        @Override public boolean equals(final Object value) { return this == value; }
        @Override public int hashCode() { return size; }

    } // end of nested class Unknown

    static private abstract class Constant extends CFValue {

        public final int sort;

        public Constant(final int size, final int sort) {
            super(size);
            this.sort = sort;
        }

        @Override
        public boolean isConstant() { return true; }

        public boolean isIntegral() {
            switch (sort) {
                case Type.BYTE:
                case Type.SHORT:
                case Type.INT:
                case Type.LONG:
                case Type.CHAR:
                    return true;
                default:
                    return false;
            }
        }

        public boolean isFloatingPoint() {
            return (sort == Type.FLOAT) || (sort == Type.DOUBLE);
        }

        public boolean isBoolean() {
            return (sort == Type.BOOLEAN);
        }

        /**
         *  Returns a fresh instance of an xCONST instruction that loads the value represented by this instance.
         */
        public abstract AbstractInsnNode pushInsn();

    } // end of nested class Constant


    final static private class ICst extends Constant {

        private final int v;

        public ICst(final char c) {
            super(1, Type.CHAR);
            this.v    = c;
        }

        public ICst(final byte b) {
            super(1, Type.BYTE);
            this.v    = b;
        }

        public ICst(final short s) {
            super(1, Type.SHORT);
            this.v    = s;
        }

        public ICst(final int i) {
            super(1, Type.INT);
            this.v    = i;
        }

        @Override public ICst convI() { return this;                      }
        @Override public FCst convF() { return new FCst((float)  this.v); }
        @Override public JCst convJ() { return new JCst((long)   this.v); }
        @Override public DCst convD() { return new DCst((double) this.v); }

        @Override public ICst iinc(int incr)  { return new ICst(v + incr); }
        @Override public ICst neg()   { return new ICst(-v); }

        @Override public CFValue add(CFValue value2) {
            assert size == value2.size;
            if(v == 0) {
                return value2;
            } else if(value2.isUnknown()) {
                return value2;
            } else {
                ICst that = (ICst) value2;
                return new ICst(v + that.v);
            }
        }
        @Override public CFValue sub(CFValue value2) {
            assert size == value2.size;
            if(value2.isUnknown()) {
                return value2;
            } else {
                ICst that = (ICst) value2;
                return new ICst(v - that.v);
            }
        }
        @Override public CFValue mul(CFValue value2) {
            assert size == value2.size;
            if(v == 0) {
                return this;
            } else if(v == 1) {
                return value2;
            } else if(value2.isUnknown()) {
                return value2;
            } else {
                ICst that = (ICst) value2;
                return new ICst(v * that.v);
            }
        }
        @Override public CFValue div(CFValue value2) {
            assert size == value2.size;
            if(value2.isUnknown()) {
                return value2;
            } else {
                ICst that = (ICst) value2;
                if(that.v == 0) { return Unknown.UNKNOWN_1; }
                else { return new ICst(v / that.v); }
            }
        }

        @Override public CFValue and(CFValue value2) {
            assert size == value2.size;
            CFValue result = Unknown.UNKNOWN_1;
            if(v == 0) {
                result = this;
            } else if(value2.isConstant()) {
                ICst that = (ICst)value2;
                result = new ICst(v & that.v);
            }
            return result;
        }
        @Override public CFValue or(CFValue value2) {
            assert size == value2.size;
            CFValue result = Unknown.UNKNOWN_1;
            if(v == 0xFFFFFFFF) {
                result = this;
            } else if(value2.isConstant()) {
                ICst that = (ICst)value2;
                result = new ICst(v | that.v);
            }
            return result;
        }

        @Override
        public AbstractInsnNode pushInsn() {
            if(sort == Type.BYTE) {
                return new IntInsnNode(Opcodes.BIPUSH, v);
            }
            if (sort == Type.SHORT) {
                return new IntInsnNode(Opcodes.SIPUSH, v);
            }
            switch (v) {
                case -1:
                    return new InsnNode(Opcodes.ICONST_M1);
                case  0:
                case  1:
                case  2:
                case  3:
                case  4:
                case  5:
                    return new InsnNode(Opcodes.ICONST_0 + v);
                default:
                    return new LdcInsnNode(java.lang.Integer.valueOf(v));
            }
        }

        @Override
        public boolean equals(final Object value) {
            if (!(value instanceof ICst)) {
                return false;
            }
            ICst that = (ICst) value;
            // basing ICst equality con comparing v's means we have to keep v's representing booleans, bytes, shorts, and chars in "canonical form"
            // for example, for sort == Type.CHAR, IINC shouldn't increase v past 0xffff (otherwise equals() with another ICst representing the same Char might fail).
            return (sort == that.sort) && (v == that.v);
        }

        @Override
        public int hashCode() {
            return sort + v;
        }

    } // end of nested class ICst

    final static private class FCst extends Constant {

        private final float v;

        public FCst(final float f) {
            super(1, Type.FLOAT);
            this.v    = f;
        }

        @Override public ICst convI() { return new ICst((int)    this.v); }
        @Override public FCst convF() { return this;                      }
        @Override public JCst convJ() { return new JCst((long)   this.v); }
        @Override public DCst convD() { return new DCst((double) this.v); }

        @Override public FCst iinc(int incr)  { throw new UnsupportedOperationException(); }
        @Override public FCst neg()   { return new FCst(-v); }

        @Override public CFValue add(CFValue value2) {
            assert size == value2.size;
            // we don't try to guess addition of positive vs negative zero and unknown, just return unknown.
            if(value2.isUnknown()) {
                return value2;
            } else {
                FCst that = (FCst) value2;
                return new FCst(v + that.v);
            }
        }
        @Override public CFValue sub(CFValue value2) {
            assert size == value2.size;
            // we don't try to guess subtraction of positive vs negative zero and unknown, just return unknown.
            if(value2.isUnknown()) {
                return value2;
            } else {
                FCst that = (FCst) value2;
                return new FCst(v - that.v);
            }
        }
        @Override public CFValue mul(CFValue value2) {
            assert size == value2.size;
            if(value2.isUnknown()) {
                return value2;
            } else {
                FCst that = (FCst) value2;
                return new FCst(v * that.v);
            }
        }
        /**
         * Quoting from the JVMS:
         *   Despite the fact that overflow, underflow, division by zero, or loss of precision may occur,
         *   execution of an fdiv instruction never throws a runtime exception.
         */
        @Override public CFValue div(CFValue value2) {
            assert size == value2.size;
            if(value2.isUnknown()) {
                return value2;
            } else {
                FCst that = (FCst) value2;
                return new FCst(v / that.v);
            }
        }

        @Override public CFValue and(CFValue value2) { throw new UnsupportedOperationException(); }
        @Override public CFValue or(CFValue value2)  { throw new UnsupportedOperationException(); }

        @Override
        public AbstractInsnNode pushInsn() {
            // can't test (v == 0f) because it may be -0.0f after all, and FCONST_0 won't make it.
            // Anyway, ASM will compact an LdcInsnNode into InsnNode if possible.
            return new LdcInsnNode(java.lang.Float.valueOf(v));
        }

        @Override
        public boolean equals(final Object value) {
            if (!(value instanceof FCst)) {
                return false;
            }
            FCst that = (FCst) value;
            return (sort == that.sort) && (v == that.v);
        }

        @Override
        public int hashCode() {
            return java.lang.Float.floatToIntBits(v);
        }

    } // end of nested class FCst

    final static private class JCst extends Constant {

        private final long  v;

        public JCst(final long j) {
            super(2, Type.LONG);
            this.v    = j;
        }

        @Override public ICst convI() { return new ICst((int)    this.v); }
        @Override public FCst convF() { return new FCst((float)  this.v); }
        @Override public JCst convJ() { return this;                      }
        @Override public DCst convD() { return new DCst((double) this.v); }

        @Override public JCst iinc(int incr)  { throw new UnsupportedOperationException(); }
        @Override public JCst neg()   { return new JCst(-v); }

        @Override public CFValue add(CFValue value2) {
            assert size == value2.size;
            if(v == 0L) {
                return value2;
            } else if(value2.isUnknown()) {
                return value2;
            } else {
                JCst that = (JCst) value2;
                return new JCst(v + that.v);
            }
        }
        @Override public CFValue sub(CFValue value2) {
            assert size == value2.size;
            if(value2.isUnknown()) {
                return value2;
            } else {
                JCst that = (JCst) value2;
                return new JCst(v - that.v);
            }
        }
        @Override public CFValue mul(CFValue value2) {
            assert size == value2.size;
            if(v == 0L) {
                return this;
            } else if(v == 1L) {
                return value2;
            } else if(value2.isUnknown()) {
                return value2;
            } else {
                JCst that = (JCst) value2;
                return new JCst(v * that.v);
            }
        }
        @Override public CFValue div(CFValue value2) {
            assert size == value2.size;
            if(value2.isUnknown()) {
                return value2;
            } else {
                JCst that = (JCst) value2;
                if(that.v == 0L) { return Unknown.UNKNOWN_2; }
                else { return new JCst(v / that.v); }
            }
        }

        @Override public CFValue and(CFValue value2) {
            assert size == value2.size;
            CFValue result = Unknown.UNKNOWN_2;
            if(v == 0) {
                result = this;
            } else if(value2.isConstant()) {
                JCst that = (JCst)value2;
                result = new JCst(v & that.v);
            }
            return result;
        }
        @Override public CFValue or(CFValue value2) {
            assert size == value2.size;
            CFValue result = Unknown.UNKNOWN_2;
            if(v == 0xFFFFFFFFFFFFFFFFL) {
                result = this;
            } else if(value2.isConstant()) {
                JCst that = (JCst)value2;
                result = new JCst(v | that.v);
            }
            return result;
        }

        @Override
        public AbstractInsnNode pushInsn() {
            if(v == 0L) { return new InsnNode(Opcodes.LCONST_0); }
            if(v == 1L) { return new InsnNode(Opcodes.LCONST_1); }
            return new LdcInsnNode(java.lang.Long.valueOf(v));
        }

        @Override
        public boolean equals(final Object value) {
            if (!(value instanceof JCst)) {
                return false;
            }
            JCst that = (JCst) value;
            return (sort == that.sort) && (v == that.v);
        }

        @Override
        public int hashCode() {
            return (int)v;
        }

    } // end of nested class JCst

    static private class DCst extends Constant {

        private final double v;

        public DCst(final double d) {
            super(2, Type.DOUBLE);
            this.v    = d;
        }

        @Override public ICst convI() { return new ICst((int)    this.v); }
        @Override public FCst convF() { return new FCst((float)  this.v); }
        @Override public JCst convJ() { return new JCst((long)   this.v); }
        @Override public DCst convD() { return this;                      }

        @Override public DCst iinc(int incr)  { throw new UnsupportedOperationException(); }
        @Override public DCst neg()   { return new DCst(-v); }

        @Override public CFValue add(CFValue value2) {
            assert size == value2.size;
            // we don't try to guess addition of positive vs negative zero and unknown, just return unknown.
            if(value2.isUnknown()) {
                return value2;
            } else {
                DCst that = (DCst) value2;
                return new DCst(v + that.v);
            }
        }
        @Override public CFValue sub(CFValue value2) {
            assert size == value2.size;
            // we don't try to guess subtraction of positive vs negative zero and unknown, just return unknown.
            if(value2.isUnknown()) {
                return value2;
            } else {
                DCst that = (DCst) value2;
                return new DCst(v - that.v);
            }
        }
        @Override public CFValue mul(CFValue value2) {
            assert size == value2.size;
            if(value2.isUnknown()) {
                return value2;
            } else {
                DCst that = (DCst) value2;
                return new DCst(v * that.v);
            }
        }
        /**
         * Quoting from the JVMS:
         *   Despite the fact that overflow, underflow, division by zero, or loss of precision may occur,
         *   execution of an fdiv instruction never throws a runtime exception.
         */
        @Override public CFValue div(CFValue value2) {
            assert size == value2.size;
            if(value2.isUnknown()) {
                return value2;
            } else {
                DCst that = (DCst) value2;
                return new DCst(v / that.v);
            }
        }

        @Override public CFValue and(CFValue value2) { throw new UnsupportedOperationException(); }
        @Override public CFValue or(CFValue value2)  { throw new UnsupportedOperationException(); }

        @Override
        public AbstractInsnNode pushInsn() {
            // can't test (v == 0d) because it may be -0.0d after all, and DCONST_0 won't make it.
            // Anyway, ASM will compact an LdcInsnNode into InsnNode if possible.
            return new LdcInsnNode(java.lang.Double.valueOf(v));
        }

        @Override
        public boolean equals(final Object value) {
            if (!(value instanceof DCst)) {
                return false;
            }
            DCst that = (DCst) value;
            return (sort == that.sort) && (v == that.v);
        }

        @Override
        public int hashCode() {
            return (int)java.lang.Double.doubleToLongBits(v);
        }

    } // end of nested class DCst

    static public class ConstantInterpreter extends Interpreter<CFValue> {

        public ConstantInterpreter() {
            super(ASM4);
        }

        protected ConstantInterpreter(final int api) {
            super(api);
        }

        private CFValue dunno(int size) {
            if(size == 0) {
                return null;
            }
            return (size == 1) ? Unknown.UNKNOWN_1 : Unknown.UNKNOWN_2;
        }

        private CFValue dunno(AbstractInsnNode producer) {
            return dunno(SizingUtil.getResultSize(producer));
        }

        @Override
        public CFValue newValue(final Type type) {
            if (type == Type.VOID_TYPE) {
                return null;
            }
            return dunno(type == null ? 1 : type.getSize());
        }

        static public ICst bconst(int b)     { return new ICst((byte)  b); }
        static public ICst sconst(int s)     { return new ICst((short) s); }
        static public ICst iconst(int i)     { return new ICst(i);         }

        static public FCst fconst(float  f)  { return new FCst(f); }
        static public JCst jconst(long   j)  { return new JCst(j); }
        static public DCst dconst(double d)  { return new DCst(d); }

        /**
         * ACONST_NULL,
         * ICONST_M1, ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5,
         * LCONST_0, LCONST_1,
         * FCONST_0, FCONST_1, FCONST_2,
         * DCONST_0, DCONST_1,
         * BIPUSH,
         * SIPUSH,
         * LDC
         */
        @Override
        public CFValue newOperation(final AbstractInsnNode insn) {
            int opc = insn.getOpcode();
            switch (opc) {

                case ACONST_NULL:
                    return Unknown.UNKNOWN_1; // use NullnessPropagator instead

                case ICONST_M1:
                    return iconst(-1);

                case ICONST_0:
                case ICONST_1:
                case ICONST_2:
                case ICONST_3:
                case ICONST_4:
                case ICONST_5:
                    return iconst(opc - ICONST_0);

                case LCONST_0:
                case LCONST_1:
                    return jconst(opc - LCONST_0);

                case FCONST_0:
                case FCONST_1:
                case FCONST_2:
                    return fconst(opc - FCONST_0);

                case DCONST_0:
                case DCONST_1:
                    return dconst(opc - DCONST_0);

                case BIPUSH:
                    return bconst(((IntInsnNode) insn).operand);

                case SIPUSH:
                    return sconst(((IntInsnNode) insn).operand);

                case LDC:
                    Object cst = ((LdcInsnNode) insn).cst;
                    if (cst instanceof Integer) {
                        return iconst(((Integer) cst).intValue());
                    } else if (cst instanceof Float) {
                        return fconst(((Float) cst).floatValue());
                    } else if (cst instanceof Long) {
                        return jconst(((Long) cst).longValue());
                    } else if (cst instanceof Double) {
                        return dconst(((Double) cst).doubleValue());
                    } else {
                        return dunno(insn);
                    }

                default:
                    return dunno(insn);
            }
        }

        /**
         * Propagates the input value through LOAD, STORE, DUP, and SWAP instructions.
         */
        @Override
        public CFValue copyOperation(final AbstractInsnNode insn, final CFValue value) {
            return value;
        }

        /**
         *  INEG, LNEG, FNEG, DNEG,
         *  IINC,
         *       I2L, I2F, I2D,
         *  L2I,      L2F, L2D,
         *  F2I, F2L,      F2D,
         *  D2I, D2L, D2F,
         *  I2B, I2C, I2S
         */
        @Override
        public CFValue unaryOperation(final AbstractInsnNode insn, final CFValue value) {
            switch (insn.getOpcode()) {
                case INEG:
                case FNEG:
                case LNEG:
                case DNEG:
                    return value.neg();

                case IINC:
                    return value.iinc(((IincInsnNode)insn).incr);

                case I2B:
                case I2C:
                case I2S:
                    if(value instanceof ICst) {
                        ICst ic = (ICst)value;
                        switch (insn.getOpcode()) {
                            case I2B: return new ICst((byte)  ic.v);
                            case I2C: return new ICst((char)  ic.v);
                            case I2S: return new ICst((short) ic.v);
                        }
                    } else {
                        return dunno(insn);
                    }

                case F2I:
                case L2I:
                case D2I:
                    return value.convI();

                case I2F:
                case L2F:
                case D2F:
                    return value.convF();

                case I2L:
                case F2L:
                case D2L:
                    return value.convJ();

                case I2D:
                case F2D:
                case L2D:
                    return value.convD();

                default:
                    return dunno(insn);
            }
        }

        /**
         * IADD, LADD, FADD, DADD,
         * ISUB, LSUB, FSUB, DSUB,
         * IMUL, LMUL, FMUL, DMUL,
         * IDIV, LDIV, FDIV, DDIV,
         * IREM, LREM, FREM, DREM,
         * ISHL, LSHL,
         * ISHR, LSHR,
         * IUSHR, LUSHR,
         * IAND, LAND,
         * IOR, LOR,
         * IXOR, LXOR,
         * LCMP,
         * FCMPL, FCMPG,
         * DCMPL, DCMPG,
         */
        @Override
        public CFValue binaryOperation(
            final AbstractInsnNode insn,
            final CFValue value1,
            final CFValue value2) {
            switch (insn.getOpcode()) {
                case IADD:
                case FADD:
                case LADD:
                case DADD:
                    return value1.add(value2);

                case ISUB:
                case FSUB:
                case LSUB:
                case DSUB:
                    return value1.sub(value2);

                case IMUL:
                case FMUL:
                case LMUL:
                case DMUL:
                    return value1.mul(value2);

                case IDIV:
                case FDIV:
                case LDIV:
                case DDIV:
                    return value1.div(value2);

                case IREM:
                case FREM:
                case LREM:
                case DREM:
                    return dunno(insn); // TODO return value1.REM(value2);

                case IAND:
                case LAND:
                    return value1.and(value2);

                case IOR:
                case LOR:
                    return value1.or(value2);

                case IXOR:
                case LXOR:
                    return dunno(insn); // TODO return value1.XOR(value2);

                case ISHL:
                case LSHL:
                    return dunno(insn); // TODO return value1.SHL(value2);

                case ISHR:
                case LSHR:
                    return dunno(insn); // TODO return value1.SHR(value2);

                case IUSHR:
                case LUSHR:
                    return dunno(insn); // TODO return value1.USHR(value2);

                case LCMP:
                    if(value1.isUnknown() || value2.isUnknown()) {
                        return Unknown.UNKNOWN_1;
                    } else {
                        JCst j1 = (JCst)value1;
                        JCst j2 = (JCst)value2;
                             if(j1.v >  j2.v) { return new ICst( 1); }
                        else if(j1.v == j2.v) { return new ICst( 0); }
                        else                  { return new ICst(-1); }
                    }

                case FCMPL:
                case DCMPL:
                    return dunno(insn); // TODO return value1.CMPL(value2);

                case FCMPG:
                case DCMPG:
                    return dunno(insn); // TODO return value1.CMPG(value2);

                default:
                    return dunno(insn);
            }
        }

        @Override
        public CFValue ternaryOperation(
            final AbstractInsnNode insn,
            final CFValue value1,
            final CFValue value2,
            final CFValue value3) {
            return dunno(insn);
        }

        @Override
        public CFValue naryOperation(final AbstractInsnNode insn, final List<? extends CFValue> values) {
            // TODO any calls to the Scala runtime for which we know it returns a primitive constant, given its arguments?
            return dunno(insn);
        }

        @Override
        public void returnOperation(AbstractInsnNode insn, CFValue value, CFValue expected) {
        }

        @Override
        public CFValue merge(final CFValue d, final CFValue w) {
            if(d == w) { return d; }
            assert d.size == w.size;
            if(d.isUnknown() || w.isUnknown()) {
                return dunno(d.size);
            }
            if (d.equals(w)) { return d; }
            else { return dunno(d.size); }
        }

    } // end of nested class ConstantInterpreter


} // end of class ConstantPropagator