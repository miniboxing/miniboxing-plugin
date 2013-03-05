/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.asm.optimiz;

import java.util.Iterator;

import scala.tools.asm.Opcodes;

import scala.tools.asm.tree.AbstractInsnNode;
import scala.tools.asm.tree.InsnList;
import scala.tools.asm.tree.JumpInsnNode;
import scala.tools.asm.tree.LabelNode;
import scala.tools.asm.tree.MethodNode;

/**
 *  Simplifies branches that need not be taken to get to their destination:
 *    (1) conditional jump followed by unconditional jump, both to the same destination.
 *    (2) (conditional or unconditional) jump to a destination that is the next program point anyway.
 *  Details in `branchOverGoto()` and in `jumpToNext()`
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 */
public class JumpReducer extends MethodTransformer {

    public JumpReducer(final MethodTransformer mt) {
        super(mt);
    }

    /** after transform() has run, this field records whether
     *  at least one pass of this transformer modified something. */
    public boolean changed = false;

    public void transform(final MethodNode mn) {
        changed = false;
        boolean keepGoing = false;
        do {
            keepGoing = jumpToNext(mn) || sameDestination(mn) || branchOverGoto(mn);
            changed   = (changed || keepGoing);
        } while(keepGoing);
        super.transform(mn);
    }

    /**
     *  Simplifies a conditional jump followed by unconditional jump (both to the same destination)
     *  by removing the conditional jump (and adjusting the stack).
     *  Returns true if rewriting was done, false otherwise.
     *
     *  Example:
     *
     *          IF<cond> BRANCH b1;
     *          // zero or more non-executable Nodes
     *          JUMP b1;
     *          ...
     *      b1: ...
     *
     *      In this case, the conditional branch is removed and the unconditional one left as-is,
     *      because among the "zero or more non-executable Nodes" a jump target may exist.
     *
     */
    private boolean sameDestination(final MethodNode mn) {
        boolean touched = false;
        InsnList insns = mn.instructions;
        Iterator<AbstractInsnNode> i = insns.iterator();
        while (i.hasNext()) {
            AbstractInsnNode insn = i.next();
            if (Util.isCondJump(insn)) {
                AbstractInsnNode nxtinsn = Util.execInsnAfter(insn);
                if(nxtinsn != null && Util.isGOTO(nxtinsn)) {
                    JumpInsnNode jin = (JumpInsnNode) insn;
                    LabelNode xLabel = jin.label;
                    LabelNode yLabel = ((JumpInsnNode) nxtinsn).label;
                    if(Util.denoteSameTarget(xLabel, yLabel)) {
                        removeJumpAndAdjustStack(mn, jin);
                        touched = true;
                    }
                }
            }
        }
        return touched;
    }

    /**
     *  Removes a (conditional or unconditional) jump to a destination that is the next program point anyway.
     *  In the "conditional" case, DROP instructions are added to make up for the removed instruction.
     *  Returns true if rewriting was done, false otherwise.
     *
     *  Example of (2), case (a)
     *
     *          JUMP b1;
     *          // zero or more non-executable Nodes
     *      b1: ...
     *
     *
     *  Example of (2), case (b)
     *
     *          IF<cond> BRANCH b1;
     *          // zero or more non-executable Nodes
     *      b1: ...
     *
     */
    private boolean jumpToNext(final MethodNode mn) {
        boolean touched = false;
        InsnList insns = mn.instructions;
        Iterator<AbstractInsnNode> i = insns.iterator();
        while (i.hasNext()) {
            AbstractInsnNode insn = i.next();
            boolean isJumpWeLike = (Util.isCondJump(insn) || Util.isGOTO(insn));
            if (isJumpWeLike) {
                AbstractInsnNode nxtinsn = Util.execInsnAfter(insn);
                JumpInsnNode jin = (JumpInsnNode) insn;
                if(nxtinsn != null && Util.insnLabelledBy(jin.label) == nxtinsn) {
                    removeJumpAndAdjustStack(mn, jin);
                    touched = true;
                }
            }
        }
        return touched;
    }

    private void removeJumpAndAdjustStack(MethodNode mn, JumpInsnNode insn) {

        InsnList insns = mn.instructions;
        int opc = insn.getOpcode();
        if(opc >= Opcodes.IFEQ && opc <= Opcodes.IFGE) {
            // drop the I on the stack
            insns.insert(insn, Util.getDrop(1));
        } else if(opc >= Opcodes.IF_ICMPEQ && opc <= Opcodes.IF_ICMPLE) {
            // drop the I, I on the stack
            insns.insert(insn, Util.getDrop(1));
            insns.insert(insn, Util.getDrop(1));
        } else if(opc == Opcodes.IF_ACMPEQ || opc == Opcodes.IF_ACMPNE) {
            // drop the ref, ref on the stack
            insns.insert(insn, Util.getDrop(1));
            insns.insert(insn, Util.getDrop(1));
        } else if(opc == Opcodes.IFNULL || opc == Opcodes.IFNONNULL) {
            // drop the ref on the stack
            insns.insert(insn, Util.getDrop(1));
        } else {
            assert insn.getOpcode() == Opcodes.GOTO;
        }
        insns.remove(insn);

    }

    /*
     * Code pattern to detect:
     *
     *         if<cond> L1 // no matter whether L1 and L2 differ or not
     *         goto L2     // no executable nor LabelNodes in between this and the previous
     *     L1:             // no executable nor LabelNodes in between this and the previous
     *
     * Rewritten into:
     *
     *         if<!cond> L2
     *     L1:
     *
     * Returns true if rewriting was done, false otherwise.
     *
     */
    private boolean branchOverGoto(final MethodNode mn) {
        boolean touched = false;
        InsnList insns = mn.instructions;
        Iterator<AbstractInsnNode> i = insns.iterator();
        while (i.hasNext()) {
            AbstractInsnNode insn = i.next();
            if (Util.isCondJump(insn)) {
                JumpInsnNode condJump = (JumpInsnNode)insn;
                AbstractInsnNode uncondJump = Util.execInsnOrLabelAfter(insn);
                if(Util.isGOTO(uncondJump)) {
                    AbstractInsnNode dest = Util.execInsnOrLabelAfter(uncondJump);
                    if(Util.isLabel(dest)) {
                        LabelNode destLabel = (LabelNode)dest;
                        if(Util.denoteSameTarget(condJump.label, destLabel)) {

                            int negatedOpc = -1;
                            switch (condJump.getOpcode()) {

                                case Opcodes.IFEQ: negatedOpc = Opcodes.IFNE; break;
                                case Opcodes.IFNE: negatedOpc = Opcodes.IFEQ; break;

                                case Opcodes.IFLT: negatedOpc = Opcodes.IFGE; break;
                                case Opcodes.IFGE: negatedOpc = Opcodes.IFLT; break;

                                case Opcodes.IFGT: negatedOpc = Opcodes.IFLE; break;
                                case Opcodes.IFLE: negatedOpc = Opcodes.IFGT; break;

                                case Opcodes.IF_ICMPEQ: negatedOpc = Opcodes.IF_ICMPNE; break;
                                case Opcodes.IF_ICMPNE: negatedOpc = Opcodes.IF_ICMPEQ; break;

                                case Opcodes.IF_ICMPLT: negatedOpc = Opcodes.IF_ICMPGE; break;
                                case Opcodes.IF_ICMPGE: negatedOpc = Opcodes.IF_ICMPLT; break;

                                case Opcodes.IF_ICMPGT: negatedOpc = Opcodes.IF_ICMPLE; break;
                                case Opcodes.IF_ICMPLE: negatedOpc = Opcodes.IF_ICMPGT; break;

                                case Opcodes.IF_ACMPEQ: negatedOpc = Opcodes.IF_ACMPNE; break;
                                case Opcodes.IF_ACMPNE: negatedOpc = Opcodes.IF_ACMPEQ; break;

                                case Opcodes.IFNULL:    negatedOpc = Opcodes.IFNONNULL; break;
                                case Opcodes.IFNONNULL: negatedOpc = Opcodes.IFNULL;    break;

                            }

                            assert negatedOpc != -1;
                            JumpInsnNode updCondInsn = new JumpInsnNode(negatedOpc, ((JumpInsnNode)uncondJump).label);

                            mn.instructions.set(condJump, updCondInsn);
                            mn.instructions.remove(uncondJump);

                            touched = true;
                        }
                    }
                }
            }
        }
        return touched;
    }

}
