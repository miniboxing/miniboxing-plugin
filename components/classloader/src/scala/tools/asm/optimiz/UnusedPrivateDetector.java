/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.asm.optimiz;

import java.util.HashSet;
import java.util.Set;
import java.util.Queue;
import java.util.Iterator;
import java.util.LinkedList;

import scala.tools.asm.Opcodes;

import scala.tools.asm.tree.FieldNode;
import scala.tools.asm.tree.MethodNode;
import scala.tools.asm.tree.ClassNode;
import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.MethodInsnNode;
import scala.tools.asm.tree.FieldInsnNode;

/**
 *  This class transformer detects usages of private members of the ClassNode to transform()
 *  (private members includes fields, methods, or constructors; be they static or instance).
 *  Those usages are detected by visiting the class' public and protected methods and constructors
 *  as well as any private methods or constructors transitively reachable.
 *
 *  Those private members for which no usages are found are elided,
 *  but that decision is left to the invoker.
 *
 *  If elided, any code using, say, reflection or invokedynamic or a methodhandle constant
 *  referring to an (otherwise unused) private member, will fail after this transformer is run.
 *
 *  Private methods and fields, as e.g. needed by java.io.Serializable or java.io.Externalizable,
 *  are elided as any other. It's recommended not to run this transformer on classes extending those interfaces.
 *  This transformer does not check that on its own.
 *
 *  It's best to run this transformer after constant propagation has run
 *  (ie after redundant usages of private members have been replaced by constants).
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 */
public class UnusedPrivateDetector implements Opcodes {

    public Set<FieldNode>  elidableStaticFields    = new HashSet<FieldNode>();
    public Set<FieldNode>  elidableInstanceFields  = new HashSet<FieldNode>();
    public Set<MethodNode> elidableStaticMethods   = new HashSet<MethodNode>();
    public Set<MethodNode> elidableInstanceMethods = new HashSet<MethodNode>();

    /**
     * @return whether any private field, methods, or constructors was elided.
     *         In the affirmative case, those elided members can be found in the fields of this class.
     */
    public boolean transform(ClassNode cnode) {

        elidableStaticFields.clear();
        elidableInstanceFields.clear();
        elidableStaticMethods.clear();
        elidableInstanceMethods.clear();

        Queue<MethodNode> toVisit = new LinkedList<MethodNode>();
        for (MethodNode m : cnode.methods) {
            if (isPrivate(m.access) && m.isLiftedMethod) {
              (isStatic(m.access) ? elidableStaticMethods : elidableInstanceMethods).add(m);
            } else {
                toVisit.add(m);
            }
        }
        for (FieldNode f : cnode.fields) {
            if (isPrivate(f.access)) {
              (isStatic(f.access) ? elidableStaticFields : elidableInstanceFields).add(f);
            }
        }

        String cname = cnode.name;

        while(!toVisit.isEmpty()) {

            if(nothingElided()) {
                return false;
            }

            Iterator<AbstractInsnNode> iter = toVisit.remove().instructions.iterator();
            while(iter.hasNext()) {
                AbstractInsnNode insn = iter.next();
                switch (insn.getType()) {

                    case AbstractInsnNode.FIELD_INSN:
                        FieldNode inUsePrivateField = lookupPrivate((FieldInsnNode)insn, cname);
                        if(inUsePrivateField != null) {
                            if(isStatic(inUsePrivateField.access)) {
                                elidableStaticFields.remove(inUsePrivateField);
                            } else {
                                elidableInstanceFields.remove(inUsePrivateField);
                            }
                        }
                        break;

                    case AbstractInsnNode.METHOD_INSN:
                        MethodNode inUsePrivateMethod = lookupPrivate((MethodInsnNode)insn, cname);
                        if(inUsePrivateMethod != null) {
                            if(isStatic(inUsePrivateMethod.access)) {
                                if(elidableStaticMethods.contains(inUsePrivateMethod)) {
                                    toVisit.add(inUsePrivateMethod);
                                }
                                elidableStaticMethods.remove(inUsePrivateMethod);
                            } else {
                                if(elidableInstanceMethods.contains(inUsePrivateMethod)) {
                                    toVisit.add(inUsePrivateMethod);
                                }
                                elidableInstanceMethods.remove(inUsePrivateMethod);
                            }
                        }
                        break;

                    default:
                        // TODO handle LDC of MethodHandle
                        break;
                }
            }
        }

        return !nothingElided();
    }

    private boolean nothingElided() {

        return (
            elidableStaticFields.isEmpty()   &&
            elidableInstanceFields.isEmpty() &&
            elidableStaticMethods.isEmpty()  &&
            elidableInstanceMethods.isEmpty()
        );

    }

    private boolean isPrivate(int access) {
        return (access & ACC_PRIVATE) != 0;
    }

    private boolean isStatic(int access) {
        return (access & ACC_STATIC) != 0;
    }

    private FieldNode lookupPrivate(FieldInsnNode fi, String cname) {
        if(fi.owner.equals(cname)) {
            boolean isStatic = fi.getOpcode() == GETSTATIC || fi.getOpcode() == PUTSTATIC;
            Set<FieldNode> candidates = (isStatic ? elidableStaticFields : elidableInstanceFields);
            for(FieldNode fn : candidates) {
                if(fn.name.equals(fi.name)) {
                    return fn;
                }
            }
        }
        return null;
    }

    private MethodNode lookupPrivate(MethodInsnNode mi, String cname) {
        if(mi.owner.equals(cname)) {
            boolean isStatic = mi.getOpcode() == INVOKESTATIC;
            Set<MethodNode> candidates = (isStatic ? elidableStaticMethods : elidableInstanceMethods);
            for(MethodNode mn : candidates) {
                if(mn.name.equals(mi.name) && mn.desc.equals(mi.desc)) {
                    return mn;
                }
            }
        }
        return null;
    }

}

