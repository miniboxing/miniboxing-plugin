/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.asm.optimiz;

import scala.tools.asm.Opcodes;
import scala.tools.asm.Type;
import scala.tools.asm.Handle;

import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.LdcInsnNode;
import scala.tools.asm.tree.FieldInsnNode;
import scala.tools.asm.tree.MethodInsnNode;
import scala.tools.asm.tree.InvokeDynamicInsnNode;

import scala.tools.asm.tree.analysis.Value;
import scala.tools.asm.tree.analysis.Interpreter;
import scala.tools.asm.tree.analysis.AnalyzerException;

/**
 *  Brings together in one place the size-computation that can be found e.g. in SourceInterpreter.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 */
public class SizingUtil implements Opcodes {

    static public int getResultSize(final AbstractInsnNode insn) {
        int size;
        switch (insn.getOpcode()) {
            case Opcodes.ACONST_NULL:
                size = 1;
                break;
            case Opcodes.ICONST_M1:
            case Opcodes.ICONST_0:
            case Opcodes.ICONST_1:
            case Opcodes.ICONST_2:
            case Opcodes.ICONST_3:
            case Opcodes.ICONST_4:
            case Opcodes.ICONST_5:
                size = 1;
                break;
            case Opcodes.LCONST_0:
            case Opcodes.LCONST_1:
                size = 2;
                break;
            case Opcodes.FCONST_0:
            case Opcodes.FCONST_1:
            case Opcodes.FCONST_2:
                size = 1;
                break;
            case Opcodes.DCONST_0:
            case Opcodes.DCONST_1:
                size = 2;
                break;
            case Opcodes.BIPUSH:
            case Opcodes.SIPUSH:
                size = 1;
                break;
            case Opcodes.LDC:
                Object cst = ((LdcInsnNode) insn).cst;
                if (cst instanceof Integer) {
                    size = 1;
                } else if (cst instanceof Float) {
                    size = 1;
                } else if (cst instanceof Long) {
                    size = 2;
                } else if (cst instanceof Double) {
                    size = 2;
                } else if (cst instanceof String) {
                    size = 1;
                } else if (cst instanceof Type) {
                    int sort = ((Type) cst).getSort();
                    if (sort == Type.OBJECT || sort == Type.ARRAY) {
                        size = 1;
                    } else if (sort == Type.METHOD) {
                        size = 1;
                    } else {
                        throw new IllegalArgumentException("Illegal LDC constant " + cst);
                    }
                } else if (cst instanceof Handle) {
                    size = 1;
                } else {
                    throw new IllegalArgumentException("Illegal LDC constant " + cst);
                }
                break;
            case Opcodes.ILOAD:
            case Opcodes.FLOAD:
            case Opcodes.ALOAD:
                size = 1;
                break;
            case Opcodes.LLOAD:
            case Opcodes.DLOAD:
                size = 2;
                break;
            case Opcodes.IALOAD:
            case Opcodes.FALOAD:
            case Opcodes.AALOAD:
            case Opcodes.BALOAD:
            case Opcodes.CALOAD:
            case Opcodes.SALOAD:
                size = 1;
                break;
            case Opcodes.LALOAD:
            case Opcodes.DALOAD:
                size = 2;
                break;
            case Opcodes.ISTORE:
            case Opcodes.LSTORE:
            case Opcodes.FSTORE:
            case Opcodes.DSTORE:
            case Opcodes.ASTORE:
                size = 0;
                break;
            case Opcodes.IASTORE:
            case Opcodes.LASTORE:
            case Opcodes.FASTORE:
            case Opcodes.DASTORE:
            case Opcodes.AASTORE:
            case Opcodes.BASTORE:
            case Opcodes.CASTORE:
            case Opcodes.SASTORE:
                size = 0;
                break;
            case Opcodes.POP:
            case Opcodes.POP2:
                size = 0;
                break;
            case Opcodes.DUP:
            case Opcodes.DUP_X1:
            case Opcodes.DUP_X2:
            case Opcodes.DUP2:
            case Opcodes.DUP2_X1:
            case Opcodes.DUP2_X2:
            case Opcodes.SWAP:
                throw new IllegalArgumentException("Can't compute the size of DUP<sthg> without knowing what's on stack top");
            case Opcodes.IADD:
            case Opcodes.FADD:
                size = 1;
                break;
            case Opcodes.LADD:
            case Opcodes.DADD:
                size = 2;
                break;
            case Opcodes.ISUB:
            case Opcodes.FSUB:
                size = 1;
                break;
            case Opcodes.LSUB:
            case Opcodes.DSUB:
                size = 2;
                break;
            case Opcodes.IMUL:
            case Opcodes.FMUL:
                size = 1;
                break;
            case Opcodes.LMUL:
            case Opcodes.DMUL:
                size = 2;
                break;
            case Opcodes.IDIV:
            case Opcodes.FDIV:
                size = 1;
                break;
            case Opcodes.LDIV:
            case Opcodes.DDIV:
                size = 2;
                break;
            case Opcodes.IREM:
            case Opcodes.FREM:
                size = 1;
                break;
            case Opcodes.LREM:
            case Opcodes.DREM:
                size = 2;
                break;
            case Opcodes.INEG:
            case Opcodes.FNEG:
                size = 1;
                break;
            case Opcodes.LNEG:
            case Opcodes.DNEG:
                size = 2;
                break;
            case Opcodes.ISHL:
            case Opcodes.ISHR:
                size = 1;
                break;
            case Opcodes.LSHL:
            case Opcodes.LSHR:
                size = 2;
                break;
            case Opcodes.IUSHR:
                size = 1;
                break;
            case Opcodes.LUSHR:
                size = 2;
                break;
            case Opcodes.IAND:
            case Opcodes.IOR:
            case Opcodes.IXOR:
                size = 1;
                break;
            case Opcodes.LAND:
            case Opcodes.LOR:
            case Opcodes.LXOR:
                size = 2;
                break;
            case Opcodes.IINC:
                size = 1;
                break;
            case Opcodes.I2F:
            case Opcodes.L2I:
            case Opcodes.L2F:
            case Opcodes.F2I:
            case Opcodes.D2I:
            case Opcodes.D2F:
            case Opcodes.I2B:
            case Opcodes.I2C:
            case Opcodes.I2S:
                size = 1;
                break;
            case Opcodes.I2L:
            case Opcodes.I2D:
            case Opcodes.L2D:
            case Opcodes.F2L:
            case Opcodes.F2D:
            case Opcodes.D2L:
                size = 2;
                break;
            case Opcodes.LCMP:
            case Opcodes.FCMPL:
            case Opcodes.FCMPG:
            case Opcodes.DCMPL:
            case Opcodes.DCMPG:
                size = 1;
                break;
            case Opcodes.IFEQ:
            case Opcodes.IFNE:
            case Opcodes.IFLT:
            case Opcodes.IFGE:
            case Opcodes.IFGT:
            case Opcodes.IFLE:
                size = 0;
                break;
            case Opcodes.IF_ICMPEQ:
            case Opcodes.IF_ICMPNE:
            case Opcodes.IF_ICMPLT:
            case Opcodes.IF_ICMPGE:
            case Opcodes.IF_ICMPGT:
            case Opcodes.IF_ICMPLE:
            case Opcodes.IF_ACMPEQ:
            case Opcodes.IF_ACMPNE:
                size = 0;
                break;
            case Opcodes.GOTO:
                size = 0;
                break;
            case Opcodes.JSR:
                throw new IllegalArgumentException("Subroutines are not supported.");
            case Opcodes.RET:
                size = 0;
                break;
            case Opcodes.TABLESWITCH:
            case Opcodes.LOOKUPSWITCH:
                size = 0;
                break;
            case Opcodes.IRETURN:
            case Opcodes.FRETURN:
            case Opcodes.ARETURN:
                size = 1;
                break;
            case Opcodes.LRETURN:
            case Opcodes.DRETURN:
                size = 2;
                break;
            case Opcodes.RETURN:
                size = 0;
                break;
            case Opcodes.GETSTATIC:
                size = Type.getType(((FieldInsnNode) insn).desc).getSize();
                break;
            case Opcodes.PUTSTATIC:
                size = 0;
                break;
            case Opcodes.GETFIELD:
                size = Type.getType(((FieldInsnNode) insn).desc).getSize();
                break;
            case Opcodes.PUTFIELD:
                size = 0;
                break;
            case Opcodes.INVOKEVIRTUAL:
            case Opcodes.INVOKESPECIAL:
            case Opcodes.INVOKESTATIC:
            case Opcodes.INVOKEINTERFACE: {
                String desc = ((MethodInsnNode) insn).desc;
                Type rt = Type.getReturnType(desc);
                if (rt == Type.VOID_TYPE) {
                    size = 0;
                } else {
                    size = rt.getSize();
                }
                break;
            }
            case Opcodes.INVOKEDYNAMIC: {
                String desc = ((InvokeDynamicInsnNode) insn).desc;
                Type rt = Type.getReturnType(desc);
                if (rt == Type.VOID_TYPE) {
                    size = 0;
                } else {
                    size = rt.getSize();
                }
                break;
            }
            case Opcodes.NEW:
                size = 1;
                break;
            case Opcodes.NEWARRAY:
            case Opcodes.ANEWARRAY:
            case Opcodes.ARRAYLENGTH:
                size = 1;
                break;
            case Opcodes.ATHROW:
                size = 0;
                break;
            case Opcodes.CHECKCAST:
            case Opcodes.INSTANCEOF:
                size = 1;
                break;
            case Opcodes.MONITORENTER:
            case Opcodes.MONITOREXIT:
                size = 0;
                break;
            case Opcodes.MULTIANEWARRAY:
                size = 1;
                break;
            case Opcodes.IFNULL:
            case Opcodes.IFNONNULL:
                size = 0;
                break;
            default:
                size = 1;
        }
        return size;
    }

}
