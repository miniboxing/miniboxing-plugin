/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.asm.optimiz;

import scala.tools.asm.Opcodes;
import scala.tools.asm.Type;
import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.MethodInsnNode;
import scala.tools.asm.tree.FieldInsnNode;

/**
 *  Utilities about bytecode specific to the Scala Standard Library.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class SSLUtil {

    // ------------------------------------------------------------------------
    // utilities made available to clients
    // ------------------------------------------------------------------------

    public static boolean isSideEffectFree(final AbstractInsnNode producer) {
        if(isSideEffectFreeCall(producer) || isSideEffectFreeGETSTATIC(producer)) return true;
        return false;
    }

    public static boolean isSideEffectFreeGETSTATIC(final AbstractInsnNode producer) {
        if(producer.getType() == AbstractInsnNode.FIELD_INSN) {
            FieldInsnNode fi = (FieldInsnNode)producer;
            if("scala/runtime/BoxedUnit".equals(fi.owner)) {
                return "UNIT".equals(fi.name);
            }
            if("scala/Unit$".equals(fi.owner)) {
                return "MODULE$".equals(fi.name); // SI-6527
            }
            if("scala/collection/immutable/Nil$".equals(fi.owner)) {
                return "MODULE$".equals(fi.name);
            }
        }
        return false;
    }

    public static boolean isSideEffectFreeCall(final AbstractInsnNode producer) {
        if(isScalaUnBox(producer) || isScalaBox(producer)) return true;
        if(Util.isJavaBox(producer)) return true; // Java unbox isn't "side effect free" because on null it NPEs.
        return false;
    }

    public static boolean isScalaUnBox(final AbstractInsnNode insn) {
      if(insn.getType()   != AbstractInsnNode.METHOD_INSN ||
         insn.getOpcode() != Opcodes.INVOKESTATIC) {
        return false;
      }

      return isScalaUnBoxCall((MethodInsnNode)insn);
    }

    public static boolean isScalaUnBoxCall(final MethodInsnNode mi) {

        if(!mi.owner.equals(BoxesRunTime)) return false;

        Type retType = Type.getReturnType(mi.desc);

        switch (retType.getSort()) {
            case Type.BOOLEAN: return "unboxToBoolean".equals(mi.name) && "(Ljava/lang/Object;)Z".equals(mi.desc);
            case Type.BYTE:    return "unboxToByte".equals(mi.name)    && "(Ljava/lang/Object;)B".equals(mi.desc);
            case Type.CHAR:    return "unboxToChar".equals(mi.name)    && "(Ljava/lang/Object;)C".equals(mi.desc);
            case Type.SHORT:   return "unboxToShort".equals(mi.name)   && "(Ljava/lang/Object;)S".equals(mi.desc);
            case Type.INT:     return "unboxToInt".equals(mi.name)     && "(Ljava/lang/Object;)I".equals(mi.desc);
            case Type.LONG:    return "unboxToLong".equals(mi.name)    && "(Ljava/lang/Object;)J".equals(mi.desc);
            case Type.FLOAT:   return "unboxToFloat".equals(mi.name)   && "(Ljava/lang/Object;)F".equals(mi.desc);
            case Type.DOUBLE:  return "unboxToDouble".equals(mi.name)  && "(Ljava/lang/Object;)D".equals(mi.desc);

            default: return false;
        }

    }

    public static boolean isScalaBox(final AbstractInsnNode insn) {
      if(insn.getType()   != AbstractInsnNode.METHOD_INSN ||
         insn.getOpcode() != Opcodes.INVOKESTATIC) {
        return false;
      }

      return isScalaBoxCall((MethodInsnNode)insn);
    }

    public static boolean isScalaBoxCall(final MethodInsnNode mi) {

        if(!mi.owner.equals(BoxesRunTime)) return false;

        Type[] argTs = Type.getArgumentTypes(mi.desc);
        if(argTs.length != 1) return false;

        switch (argTs[0].getSort()) {
            case Type.BOOLEAN: return "boxToBoolean".equals(mi.name)   && "(Z)Ljava/lang/Boolean;".equals(mi.desc);
            case Type.BYTE:    return "boxToByte".equals(mi.name)      && "(B)Ljava/lang/Byte;".equals(mi.desc);
            case Type.CHAR:    return "boxToCharacter".equals(mi.name) && "(C)Ljava/lang/Character;".equals(mi.desc);
            case Type.SHORT:   return "boxToShort".equals(mi.name)     && "(S)Ljava/lang/Short;".equals(mi.desc);
            case Type.INT:     return "boxToInteger".equals(mi.name)   && "(I)Ljava/lang/Integer;".equals(mi.desc);
            case Type.LONG:    return "boxToLong".equals(mi.name)      && "(J)Ljava/lang/Long;".equals(mi.desc);
            case Type.FLOAT:   return "boxToFloat".equals(mi.name)     && "(F)Ljava/lang/Float;".equals(mi.desc);
            case Type.DOUBLE:  return "boxToDouble".equals(mi.name)    && "(D)Ljava/lang/Double;".equals(mi.desc);

            default: return false;
        }

    }

    // ------------------------------------------------------------------------
    // internal methods
    // ------------------------------------------------------------------------

    private static String BoxesRunTime = "scala/runtime/BoxesRunTime";

}
