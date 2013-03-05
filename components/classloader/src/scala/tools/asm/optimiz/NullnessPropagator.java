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
import scala.tools.asm.tree.InvokeDynamicInsnNode;
import scala.tools.asm.tree.MethodInsnNode;
import scala.tools.asm.tree.JumpInsnNode;
import scala.tools.asm.tree.TypeInsnNode;
import scala.tools.asm.tree.InsnNode;
import scala.tools.asm.tree.VarInsnNode;
import scala.tools.asm.tree.InsnList;

import scala.tools.asm.tree.analysis.AnalyzerException;
import scala.tools.asm.tree.analysis.Analyzer;
import scala.tools.asm.tree.analysis.Frame;
import scala.tools.asm.tree.analysis.Value;
import scala.tools.asm.tree.analysis.Interpreter;

/**
 *  Infers null resp. non-null reaching certain program points, using that information to:
 *
 *    (a) simplify control-flow when a conditional jump will always be taken or avoided,
 *        for three classes of instructions:
 *          (a.1) IF_ACMPEQ, IF_ACMPNE
 *          (a.2) IFNULL,    IFNONNULL
 *          (a.3) INSTANCEOF
 *
 *    (b) simplify an ALOAD for a local-var known to contain null (ACONST_NULL replaces it).
 *        This might enable further reductions (e.g., dead-store elimination).
 *
 *    (c) Scala unbox of null is replaced by a 0 load of the appropriate sort (I, L, F, or D).
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 */
public class NullnessPropagator {

    /** after transform() has run, this field records whether
     *  at least one pass of this transformer modified something. */
    public boolean changed = false;

    private UnreachableCode unreachCodeRemover = new UnreachableCode();

    public void transform(final String owner, final MethodNode mnode) throws AnalyzerException {

        NullnessAnalyzer nany    = NullnessAnalyzer.create();
        NullnessFrame[] frames   = nany.analyze(owner, mnode);
        AbstractInsnNode[] insns = mnode.instructions.toArray();

        changed = false;

        StatusValue value1 = null;
        StatusValue value2 = null;

        boolean alwaysTaken   = false;
        boolean alwaysAvoided = false;

        JumpInsnNode jmp = null;

        int i = 0;
        while(i < insns.length) {

            if(insns[i] != null) {

                NullnessFrame frame = frames[i];
                int opc = insns[i].getOpcode();
                switch (opc) {

                    case Opcodes.ALOAD:
                        VarInsnNode vi = (VarInsnNode)insns[i];
                        if(frame.getLocal(vi.var).isNull()) {
                            mnode.instructions.set(vi, new InsnNode(Opcodes.ACONST_NULL));
                        }
                        break;

                    case Opcodes.IF_ACMPEQ:
                    case Opcodes.IF_ACMPNE:
                        value2 = frame.getStackTop();
                        value1 = frame.peekDown(1);
                        boolean alwaysEQ   =
                                (value1.isNull() && value2.isNull());
                        boolean alwaysNE =
                                (value1.isNull()    && value2.isNonNull()) ||
                                (value1.isNonNull() && value2.isNull());
                        assert !(alwaysEQ && alwaysNE);
                        alwaysTaken =
                                (opc == Opcodes.IF_ACMPEQ && alwaysEQ) ||
                                (opc == Opcodes.IF_ACMPNE && alwaysNE);
                        alwaysAvoided =
                                (opc == Opcodes.IF_ACMPEQ && alwaysNE) ||
                                (opc == Opcodes.IF_ACMPNE && alwaysEQ);
                        jmp = (JumpInsnNode) insns[i];
                        if(alwaysTaken)        { adjustBranch(mnode, jmp, 2, true ); }
                        else if(alwaysAvoided) { adjustBranch(mnode, jmp, 2, false); }
                        break;

                    case Opcodes.INVOKESTATIC:
                        if((frame.getStackSize() > 0) && (frame.getStackTop().isNull())) {
                            MethodInsnNode mi = (MethodInsnNode)insns[i];
                            if(SSLUtil.isScalaUnBoxCall(mi)) {
                                // Scala unbox of null is 0
                                Type retType = Type.getReturnType(mi.desc);
                                int zeroLoadOpcode = -1;
                                switch (retType.getSort()) {
                                    case Type.BOOLEAN:
                                    case Type.BYTE:
                                    case Type.CHAR:
                                    case Type.SHORT:
                                    case Type.INT:
                                        zeroLoadOpcode = Opcodes.ICONST_0;
                                        break;
                                    case Type.LONG:
                                        zeroLoadOpcode = Opcodes.LCONST_0;
                                        break;
                                    case Type.FLOAT:
                                        zeroLoadOpcode = Opcodes.FCONST_0;
                                        break;
                                    case Type.DOUBLE:
                                        zeroLoadOpcode = Opcodes.DCONST_0;
                                        break;
                                }
                                mnode.instructions.insert(insns[i], new InsnNode(zeroLoadOpcode));
                                mnode.instructions.set(insns[i], new InsnNode(Opcodes.POP));
                                changed = true;
                            }
                        }
                        break;

                    case Opcodes.INSTANCEOF:
                        if(frame.getStackTop().isNull()) {
                            // drop ref, ICONST_0
                            TypeInsnNode iof = (TypeInsnNode)insns[i];
                            mnode.instructions.insert(iof, new InsnNode(Opcodes.ICONST_0));
                            mnode.instructions.insert(iof, Util.getDrop(1));
                            mnode.instructions.remove(iof);
                            changed = true;
                        }
                        break;

                    case Opcodes.IFNULL:
                    case Opcodes.IFNONNULL:
                        value1 = frame.getStackTop();
                        alwaysTaken =
                                (opc == Opcodes.IFNULL    && value1.isNull()) ||
                                (opc == Opcodes.IFNONNULL && value1.isNonNull());
                        alwaysAvoided =
                                (opc == Opcodes.IFNULL    && value1.isNonNull()) ||
                                (opc == Opcodes.IFNONNULL && value1.isNull());
                        assert !(alwaysTaken && alwaysAvoided);
                        jmp = (JumpInsnNode) insns[i];
                        if(alwaysTaken)        { adjustBranch(mnode, jmp, 1, true ); }
                        else if(alwaysAvoided) { adjustBranch(mnode, jmp, 1, false); }
                        break;

                }

            }

            i += 1;
        }

        if(changed) {
            // UnreachableCode eliminates null frames (which complicate further analyses).
            unreachCodeRemover.transform(owner, mnode);
        }

    }

    /**
     *  As part of rephrasing the conditional branching given by `insn`, it's necessary to
     *    (a) drop the arguments it would have consumed.
     *    (b) rephrase the jump instruction itself.
     *
     *  In detail,
     *    (a) involves dropping the number of references given by `drops`.
     *    (b) whether the branch is (always or never) taken is given by `doJump`,
     *        depending on which a GOTO is inserted or not.
     *
     *  As usual when inserting instructions, we adopt an instruction as "anchor" (`insns` in this case)
     *  and insert *one by one* the instructions of interest *right after the anchor*.
     *  As a result, "the instructions of interest" are inserted in the reverse order they will appear.
     *  Alternatively, an InsnList could be inserted in one go.
     *
     * */
    private void adjustBranch(MethodNode mn, JumpInsnNode insn, int drops, boolean doJump) {
        InsnList insns = mn.instructions;
        if(doJump) {
            insns.insert(insn, new JumpInsnNode(Opcodes.GOTO, insn.label));
        }
        assert drops == 1 || drops == 2;
        insns.insert(insn, Util.getDrop(1));
        if(drops == 2) {
            insns.insert(insn, Util.getDrop(1));
        }
        insns.remove(insn);
        changed = true;
    }


    public enum Nullness {
        INDOUBT_STATUS,
        NONNULL_STATUS,
        NULL_STATUS
    }


    /**
     *  A StatusValue tracks the null vs. non-null vs. in-doubt status of an abstract value.
     *  A StatusValue is immutable, yet its object identity carries additional information:
     *  the same object in, say, two local-vars indicates they refer at runtime to the same object-value,
     *  thus any change in the status of one of them should apply to the other. See markNONNULL() and markNULL().
     * */
    static private class StatusValue implements Value {

        private final int size;
        private final Nullness status;

        public StatusValue(final int size) {
            assert size == 1 || size == 2;
            this.size   = size;
            this.status = Nullness.INDOUBT_STATUS;
        }

        public StatusValue(final int size, final Nullness status) {
            this.size   = size;
            this.status = status;
        }

        public int getSize() {
            return size;
        }

        public boolean isNull()    { return status == Nullness.NULL_STATUS;    }
        public boolean isNonNull() { return status == Nullness.NONNULL_STATUS; }
        public boolean isIndoubt() { return status == Nullness.INDOUBT_STATUS; }

        public StatusValue checkedNULL()    { return new StatusValue(this.size, Nullness.NULL_STATUS);    }
        public StatusValue checkedNONNULL() { return new StatusValue(this.size, Nullness.NONNULL_STATUS); }
        public StatusValue checkedINDOUBT() { return new StatusValue(this.size, Nullness.INDOUBT_STATUS); }

        @Override
        public boolean equals(final Object value) {
            if (!(value instanceof StatusValue)) {
                return false;
            }
            StatusValue v = (StatusValue) value;
            return (size == v.size) && (status == v.status);
        }

        @Override
        public int hashCode() {
            return size + status.hashCode();
        }

    } // end of nested class StatusValue


    static public class StatusInterpreter extends Interpreter<StatusValue> implements Opcodes {

        public StatusInterpreter() {
            super(ASM4);
        }

        protected StatusInterpreter(final int api) {
            super(api);
        }

        @Override
        public StatusValue newValue(final Type type) {
            if (type == Type.VOID_TYPE) {
                return null;
            }
            return new StatusValue(type == null ? 1 : type.getSize());
        }

        public StatusValue createStatusValue(final int size) {
            return new StatusValue(size);
        }

        @Override
        public StatusValue newOperation(final AbstractInsnNode insn) throws AnalyzerException {
            final int size = SizingUtil.getResultSize(insn);
            Nullness status = Nullness.INDOUBT_STATUS;
            if (insn.getOpcode() == Opcodes.ACONST_NULL) {
                status = Nullness.NULL_STATUS;
            }
            return new StatusValue(size, status);
        }

        @Override
        public StatusValue copyOperation(final AbstractInsnNode insn, final StatusValue value) {
            return value;
        }

        @Override
        public StatusValue unaryOperation(final AbstractInsnNode insn, final StatusValue value) throws AnalyzerException {
            StatusValue sv = createStatusValue(SizingUtil.getResultSize(insn));
            switch (insn.getOpcode()) {
                case NEWARRAY:
                case ANEWARRAY:
                    return sv.checkedNONNULL();
                default:
                    return sv;
            }
        }

        @Override
        public StatusValue binaryOperation(
            final AbstractInsnNode insn,
            final StatusValue value1,
            final StatusValue value2) throws AnalyzerException {
            return createStatusValue(SizingUtil.getResultSize(insn));
        }

        @Override
        public StatusValue ternaryOperation(
            final AbstractInsnNode insn,
            final StatusValue value1,
            final StatusValue value2,
            final StatusValue value3)
        {
            return createStatusValue(1);
        }

        @Override
        public StatusValue naryOperation(final AbstractInsnNode insn, final List<? extends StatusValue> values) {
            int size;
            int opcode = insn.getOpcode();
            if (opcode == MULTIANEWARRAY) {
                size = 1;
            } else {
                String desc = (opcode == INVOKEDYNAMIC)?
                        ((InvokeDynamicInsnNode) insn).desc:
                        ((MethodInsnNode) insn).desc;
                size = Type.getReturnType(desc).getSize();
            }
            StatusValue sv = createStatusValue(size);

            switch (insn.getOpcode()) {
                case MULTIANEWARRAY:
                    return sv.checkedNONNULL();
                case INVOKEVIRTUAL:
                case INVOKESPECIAL:
                case INVOKEINTERFACE:
                    if(neverReturnsNull((MethodInsnNode) insn)) {
                        return sv.checkedNONNULL();
                    } else {
                        return sv;
                    }
                default:
                    return sv;
            }

        }

        @Override
        public void returnOperation(
            final AbstractInsnNode insn,
            final StatusValue value,
            final StatusValue expected)
        {
        }

        @Override
        public StatusValue merge(final StatusValue d, final StatusValue w) {

            if(d == w) {
                return d;
            }
            assert d.size == w.size;
            // this ensures the updated sv.status won't be seen from aliased references if any.
            StatusValue sv = new StatusValue(d.size);
            if (d.isNonNull() && w.isNonNull()) {
                sv = sv.checkedNONNULL();
            } else if (d.isNull() && w.isNull()) {
                sv = sv.checkedNULL();
            } else {
                assert sv.isIndoubt();
            }

            return sv;
        }

        /**
         *  Provided the call given by the argument completes normally,
         *  does that call always return a non-null reference?
         * */
        private boolean neverReturnsNull(final MethodInsnNode callsite) {
            if(Util.isJavaBoxCall(callsite))     return true;
            if(SSLUtil.isScalaBoxCall(callsite)) return true;
            return false;
        }

        // TODO similar to `neverReturnsNull`, but for FieldInsnNode (e.g. GETSTATIC for a module never pushes null, right?)

    } // end of nested class StatusInterpreter



    static private class NullnessAnalyzer extends Analyzer<StatusValue> {

        public static NullnessAnalyzer create() {
            return new NullnessAnalyzer(new StatusInterpreter());
        }

        public NullnessAnalyzer(final StatusInterpreter interpreter) {
            super(interpreter);
        }

        MethodNode mnode = null;
        NullnessFrame[] frames = null;

        @Override
        public NullnessFrame[] analyze(final String owner, final MethodNode mnode) throws AnalyzerException {
            this.mnode = mnode;
            Frame[] src = super.analyze(owner, mnode);
            frames = new NullnessFrame[src.length];
            System.arraycopy(src, 0, frames, 0, src.length);
            return frames;
        }

        public NullnessFrame frameAt(AbstractInsnNode insn) {
            int idx = mnode.instructions.indexOf(insn);
            return frames[idx];
        }

        /**
         * An initial value for a formal param.
         *
         * This overridable method comes handy (for example) to track as non-null the THIS reference of an instance method.
         *
         * @param isInstanceMethod
         * @param idx the index of the local-var whose abstract value we're returning.
         * @param type the type of the local-var
         * @return the created abstract value.
         */
        @Override
        public StatusValue newFormal(final boolean isInstanceMethod, final int idx, Type type) {
            assert(type != Type.VOID_TYPE);
            int size = type.getSize();
            if(isInstanceMethod && idx == 0) {
                return new StatusValue(size, Nullness.NONNULL_STATUS);
            } else {
                return new StatusValue(size, Nullness.INDOUBT_STATUS);
            }
        }

        /**
         * Constructs a new frame with the given size.
         *
         * @param nLocals the maximum number of local variables of the frame.
         * @param nStack the maximum stack size of the frame.
         * @return the created frame.
         */
        @Override
        protected NullnessFrame newFrame(final int nLocals, final int nStack) {
            return new NullnessFrame(nLocals, nStack);
        }

        /**
         * Constructs a new frame that is identical to the given frame.
         *
         * @param src a frame.
         * @return the created frame.
         */
        @Override
        protected NullnessFrame newFrame(final Frame src) {
            return new NullnessFrame(src);
        }

    } // end of nested class NullnessAnalyzer

    static private class NullnessFrame extends Frame<StatusValue> {

        /**
         * Constructs a new frame with the given size.
         *
         * @param nLocals the maximum number of local variables of the frame.
         * @param nStack the maximum stack size of the frame.
         */
        public NullnessFrame(final int nLocals, final int nStack) {
            super(nLocals, nStack);
        }

        /**
         * Constructs a new frame that is identical to the given frame.
         *
         * @param src a frame.
         */
        public NullnessFrame(final Frame src) {
            super(src);
        }

        /**
         *  In order to track the effects of an instruction beyond what the dataflow framework does, the general recipe is:
         *
         *    (1) subclass Frame, add fields to track those additional effects
         *
         *    (2) override Frame.execute(). For the opcodes of interest,
         *        collect any information deemed useful at the pre-state (ie before invoking super.execute())
         *
         *    (3) either let super.execute() perform the usual locals-and-stack update; or
         *        perform a custom update from pre-state to post-state
         *
         *    (4) update the (additional) fields used to track effects (based on the post-state).
         *
         *  Regarding NullnessFrame, the above can be seen at work as follows:
         *
         *    (a) After, say, GETFIELD we know (assuming normal ternimation) that the "receiver" object reference is not null.
         *        Similarly for array de-references, callsites targeting instance methods, and a few others.
         *
         *    (b) We can encode the thus gained information in the abstract values the dataflow manipulates
         *        (no additional fields in the Frame subclass are needed).
         *
         *    (c) A basic "pointer analysis" is also used (aliasing is detected only for references held in locals).
         *        Two locals must point to the same object whenever they hold the same StatusValue ("same" as in "object identity").
         *
         */
        @Override
        public void execute(
            final AbstractInsnNode insn,
            final Interpreter<StatusValue> interpreter) throws AnalyzerException
        {

            StatusValue ref = null;

            switch (insn.getOpcode()) {
                case Opcodes.IALOAD:
                case Opcodes.LALOAD:
                case Opcodes.FALOAD:
                case Opcodes.DALOAD:
                case Opcodes.AALOAD:
                case Opcodes.BALOAD:
                case Opcodes.CALOAD:
                case Opcodes.SALOAD:
                    ref = peekDown(1);
                    break;
                case Opcodes.IASTORE:
                case Opcodes.FASTORE:
                case Opcodes.AASTORE:
                case Opcodes.BASTORE:
                case Opcodes.CASTORE:
                case Opcodes.SASTORE:
                    ref = peekDown(2);
                    break;
                case Opcodes.LASTORE:
                case Opcodes.DASTORE:
                    /* As stated in analysis.Frame.getStackSize(), for the purposes of stack-indexing:
                     *   "Long and double values are treated as single values." */
                    ref = peekDown(2);
                    break;
                case Opcodes.GETFIELD:
                    ref = getStackTop();
                    break;
                case Opcodes.PUTFIELD:
                    ref = peekDown(1);
                    break;
                case Opcodes.INVOKEVIRTUAL:
                case Opcodes.INVOKESPECIAL:
                case Opcodes.INVOKEINTERFACE:
                    String desc = ((MethodInsnNode) insn).desc;
                    int skip = Type.getArgumentTypes(desc).length;
                    ref = peekDown(skip);
                    break;
                case Opcodes.ARRAYLENGTH:
                case Opcodes.MONITORENTER:
                case Opcodes.MONITOREXIT:
                    ref = getStackTop();
                    break;
                default:
                    ref = null;
            }

            // TODO It would be great to have computed dedicated state frames for each branch of IFNULL and IFNONNULL.
            //      For now, only the assumptions that apply to both branches are made (ie, no knowledge is gained from getStackTop()).

            super.execute(insn, interpreter);

            if(ref != null) {
                markNONNULL(ref);
            }
        }

        private void markNONNULL(StatusValue sv) {
            if(sv.isNonNull()) return;
            if(sv.isNull()) {
                // TODO inform about potential runtime NPE (if available, display source-line from LineNumberNode, see also ClassNode.sourceFile).
                markINDOUBT(sv);
                return;
            }
            StatusValue checked = sv.checkedNONNULL();
            for(int i = 0; i < locals + top; i++) {
                if(peekValue(i) == sv) {
                    pokeValue(i, checked);
                }
            }
        }

        private void markINDOUBT(StatusValue sv) {
            if(sv.isIndoubt()) return;
            StatusValue checked = sv.checkedINDOUBT();
            for(int i = 0; i < locals + top; i++) {
                if(peekValue(i) == sv) {
                    pokeValue(i, checked);
                }
            }
        }

    } // end of nested class NullnessFrame


} // end of class NullnessPropagator