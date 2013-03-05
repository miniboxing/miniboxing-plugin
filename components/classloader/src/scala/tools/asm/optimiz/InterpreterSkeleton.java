/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.asm.optimiz;

import scala.tools.asm.Handle;
import scala.tools.asm.Opcodes;
import scala.tools.asm.Type;
import scala.tools.asm.tree.*;
import scala.tools.asm.tree.analysis.Interpreter;
import scala.tools.asm.tree.analysis.Value;
import scala.tools.asm.tree.analysis.AnalyzerException;

/**
 * An {@link Interpreter} skeleton that handles instructions with primitive result,
 * so that overrides need to focus only on dataflow-analysis specifics.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 */
public abstract class InterpreterSkeleton<V extends Value> extends Interpreter<V> implements
        Opcodes
{

    public InterpreterSkeleton() {
        super(ASM4);
    }

    @Override
    public V newOperation(final AbstractInsnNode insn)
            throws AnalyzerException
    {
        switch (insn.getOpcode()) {
            case ACONST_NULL:
                return nullValue();
            case ICONST_M1:
            case ICONST_0:
            case ICONST_1:
            case ICONST_2:
            case ICONST_3:
            case ICONST_4:
            case ICONST_5:
                return intValue();
            case LCONST_0:
            case LCONST_1:
                return longValue();
            case FCONST_0:
            case FCONST_1:
            case FCONST_2:
                return floatValue();
            case DCONST_0:
            case DCONST_1:
                return doubleValue();
            case BIPUSH:
            case SIPUSH:
                return intValue();
            case LDC:
                Object cst = ((LdcInsnNode) insn).cst;
                if (cst instanceof Integer) {
                    return intValue();
                } else if (cst instanceof Float) {
                    return floatValue();
                } else if (cst instanceof Long) {
                    return longValue();
                } else if (cst instanceof Double) {
                    return doubleValue();
                } else if (cst instanceof String) {
                    return stringValue();
                } else if (cst instanceof Type) {
                    int sort = ((Type) cst).getSort();
                    if (sort == Type.OBJECT || sort == Type.ARRAY) {
                        return opLDCRefTypeValue(insn, (Type) cst);
                    } else if (sort == Type.METHOD) {
                        return opLDCMethodTypeValue(insn, (Type) cst);
                    } else {
                        throw new IllegalArgumentException("Illegal LDC constant " + cst);
                    }
                } else if (cst instanceof Handle) {
                    return opLDCHandleValue(insn, (Handle) cst);
                } else {
                    throw new IllegalArgumentException("Illegal LDC constant " + cst);
                }
            case JSR:
                throw new java.lang.UnsupportedOperationException();
            case GETSTATIC:
                return opGETSTATIC((FieldInsnNode) insn);
            case NEW:
                return opNEW((TypeInsnNode) insn);
            default:
                throw new Error("Internal error.");
        }
    }

    @Override
    public V unaryOperation(final AbstractInsnNode insn, final V value)
            throws AnalyzerException
    {
        switch (insn.getOpcode()) {
            case INEG:
            case IINC:
            case L2I:
            case F2I:
            case D2I:
            case I2B:
            case I2C:
            case I2S:
                return intValue();
            case FNEG:
            case I2F:
            case L2F:
            case D2F:
                return floatValue();
            case LNEG:
            case I2L:
            case F2L:
            case D2L:
                return longValue();
            case DNEG:
            case I2D:
            case L2D:
            case F2D:
                return doubleValue();
            case IFEQ:
            case IFNE:
            case IFLT:
            case IFGE:
            case IFGT:
            case IFLE:
            case TABLESWITCH:
            case LOOKUPSWITCH:
            case IRETURN:
            case LRETURN:
            case FRETURN:
            case DRETURN:
            case ARETURN:
                return null;
            case PUTSTATIC:
                return opPUTSTATIC((FieldInsnNode) insn, value);
            case GETFIELD:
                return opGETFIELD((FieldInsnNode) insn, value);
            case NEWARRAY:
                switch (((IntInsnNode) insn).operand) {
                    case T_BOOLEAN:
                        return newValue(arrayOf(Type.BOOLEAN_TYPE));
                    case T_CHAR:
                        return newValue(arrayOf(Type.CHAR_TYPE));
                    case T_BYTE:
                        return newValue(arrayOf(Type.BYTE_TYPE));
                    case T_SHORT:
                        return newValue(arrayOf(Type.SHORT_TYPE));
                    case T_INT:
                        return newValue(arrayOf(Type.INT_TYPE));
                    case T_FLOAT:
                        return newValue(arrayOf(Type.FLOAT_TYPE));
                    case T_DOUBLE:
                        return newValue(arrayOf(Type.DOUBLE_TYPE));
                    case T_LONG:
                        return newValue(arrayOf(Type.LONG_TYPE));
                    default:
                        throw new AnalyzerException(insn, "Invalid array type");
                }
            case ANEWARRAY:
                return opANEWARRAY((TypeInsnNode) insn);
            case ARRAYLENGTH:
                return intValue();
            case ATHROW:
                return null;
            case CHECKCAST:
                return opCHECKCAST((TypeInsnNode) insn);
            case INSTANCEOF:
                return intValue();
            case MONITORENTER:
            case MONITOREXIT:
            case IFNULL:
            case IFNONNULL:
                return null;
            default:
                throw new Error("Internal error.");
        }
    }

    @Override
    public V binaryOperation(
        final AbstractInsnNode insn,
        final V value1,
        final V value2) throws AnalyzerException
    {
        switch (insn.getOpcode()) {
            case IALOAD:
            case BALOAD:
            case CALOAD:
            case SALOAD:
            case IADD:
            case ISUB:
            case IMUL:
            case IDIV:
            case IREM:
            case ISHL:
            case ISHR:
            case IUSHR:
            case IAND:
            case IOR:
            case IXOR:
                return intValue();
            case FALOAD:
            case FADD:
            case FSUB:
            case FMUL:
            case FDIV:
            case FREM:
                return floatValue();
            case LALOAD:
            case LADD:
            case LSUB:
            case LMUL:
            case LDIV:
            case LREM:
            case LSHL:
            case LSHR:
            case LUSHR:
            case LAND:
            case LOR:
            case LXOR:
                return longValue();
            case DALOAD:
            case DADD:
            case DSUB:
            case DMUL:
            case DDIV:
            case DREM:
                return doubleValue();
            case AALOAD:
                return opAALOAD((InsnNode) insn, value1, value2);
            case LCMP:
            case FCMPL:
            case FCMPG:
            case DCMPL:
            case DCMPG:
                return intValue();
            case IF_ICMPEQ:
            case IF_ICMPNE:
            case IF_ICMPLT:
            case IF_ICMPGE:
            case IF_ICMPGT:
            case IF_ICMPLE:
            case IF_ACMPEQ:
            case IF_ACMPNE:
                return null;
            case PUTFIELD:
                return opPUTFIELD((FieldInsnNode) insn, value1, value2);
            default:
                throw new Error("Internal error.");
        }
    }

    public abstract V nullValue();
    public abstract V intValue();
    public abstract V longValue();
    public abstract V floatValue();
    public abstract V doubleValue();
    public abstract V stringValue();

    public abstract V opAALOAD(InsnNode insn, V arrayref, V index);

    public Type arrayOf(Type t) {
        String arrType = "[" + t.getDescriptor();
        return Type.getObjectType(arrType);
    }

    public abstract V opANEWARRAY(TypeInsnNode insn);
    public abstract V opNEW(TypeInsnNode insn);

    public abstract V opCHECKCAST(TypeInsnNode insn);

    public abstract V opGETFIELD(FieldInsnNode insn, V objectref);
    public abstract V opPUTFIELD(FieldInsnNode insn, V objectref, V value);

    public abstract V opGETSTATIC(FieldInsnNode insn);
    public abstract V opPUTSTATIC(FieldInsnNode insn, V value);

    public abstract V opLDCRefTypeValue(AbstractInsnNode insn,    Type cst);
    public abstract V opLDCMethodTypeValue(AbstractInsnNode insn, Type cst);
    public abstract V opLDCHandleValue(AbstractInsnNode insn,     Handle cst);

}

