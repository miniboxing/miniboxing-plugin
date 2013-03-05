/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.asm.optimiz;

import java.util.Iterator;
import java.util.ListIterator;

import scala.tools.asm.Type;
import scala.tools.asm.Opcodes;
import scala.tools.asm.tree.*;

/**
 *  Usages of local-vars comprise LOAD and STORE instructions,
 *  as well as LocalVariableNodes specifying, for debugging purposes, instruction-ranges over which variables are in scope.
 *
 *  Each usage of a local-var refers to the var in question via an index.
 *
 *  "Compacting" of non-param local-vars involves:
 *    (1) detecting gaps in the index sequence, followed by
 *    (2) re-numbering that shifts indexes to close the gaps.
 *  As already hinted, "compacting" excludes parameters. None of its usages are touched.
 *  Also, a local-var that at least gets initialized won't be removed.
 *
 *  When detecting gaps, local-var size is relevant: the 2nd "half" of a LONG or DOUBLE isn't counted as part of a gap.
 *
 *  "Re-numbering" involves:
 *    (2.a) updating those LOAD or STORE instructions referring to an old index to use instead the new index.
 *    (2.b) similarly for LocalVariableNodes.
 *    (2.c) removing those LocalVariableNode where the local var it refers to has no associated LOAD or STORE instructions.
 *    (2.d) MethodNode.maxLocals is updated.
 *
 *  This MethodTransformer:
 *    - need not be applied repeatedly, ie no further reductions will be performed on the output it produces.
 *    - should be applied at the very end of a transformation chain.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
public class LocalVarCompact extends MethodTransformer {

    public LocalVarCompact(final MethodTransformer mt) {
        super(mt);
    }

    public void transform(final MethodNode mn) {

        // In a first pass over the instruction stream, this array records which local-vars are in use.
        // An unused local-var at index idx will be shown as `renumber[idx] == -1`
        int idx = 0;
        int[] renumber = new int[mn.maxLocals];

        // we want all local-vars that denote params to survive, thus we set their corresponding elems to the var's index.
        Type[] paramTs = Type.getArgumentTypes(mn.desc);
        int numParams = paramTs.length;
        if ((mn.access & Opcodes.ACC_STATIC) == 0) {
            renumber[idx++] = 0;
        }
        for(int i = 0; i < numParams; i++) {
            renumber[idx++] = idx;
            if(paramTs[i].getSize() > 1) {
                renumber[idx++] = idx;
            }
        }

        // `numPLocals` stands for the number of locals denoting params.
        // It's in general different from numParams because the former takes sizes into account.
        // For example, an instance method with descriptor "(J)V" will have numPLocals == 3, while numParams == 2.
        int numPLocals = idx;
        while(idx < renumber.length) {
            renumber[idx++] = -1;
        }

        // find out which local-vars are in use.
        ListIterator<AbstractInsnNode> iter = mn.instructions.iterator();
        while(iter.hasNext()) {
            AbstractInsnNode current = iter.next();
            if(Util.isLOAD(current) || Util.isSTORE(current)) {
                VarInsnNode vnode = (VarInsnNode) current;
                renumber[vnode.var] = vnode.var;
                if(isWide(vnode)) {
                    renumber[vnode.var + 1] = vnode.var;
                }
            }
        }

        // after the following loop, `updMax` denotes the highest local-var-index that will remain after re-numbering.
        int updMax = numPLocals - 1;
        for(int i = numPLocals; i < renumber.length; i++) {
            if(renumber[i] != -1) {
              updMax++;
              assert renumber[i] >= updMax;
              renumber[i] = updMax;
            }
        }

        // in case no redundant local-var was found, quit early.
        if(mn.maxLocals == updMax + 1) {
            return;
        }

        compactLocalVarEntries(mn, numPLocals, renumber);

        // re-number the argument of a LOAD or STORE instruction, provided the instruction refers to a re-numbered var.
        iter = mn.instructions.iterator();
        while(iter.hasNext()) {
            AbstractInsnNode current = iter.next();
            if(Util.isLOAD(current) || Util.isSTORE(current)) {
                VarInsnNode vnode = (VarInsnNode) current;
                boolean isParam = (vnode.var < numPLocals);
                if(!isParam) {
                    assert renumber[vnode.var] != -1;
                    assert vnode.var >= renumber[vnode.var];
                    boolean shouldRenum = (vnode.var != renumber[vnode.var]);
                    if(shouldRenum) {
                        vnode.var = renumber[vnode.var];
                    }
                }
            }
        }

        // check: (number of removed local-vars plus surviving ones) add up to previous number of local-vars.
        int numM1 = 0;
        for(int i = 0; i < renumber.length; i++) {
            if(renumber[i] == -1) {
                numM1++;
            }
        }
        assert renumber.length == (updMax + 1) + numM1;
        assert updMax + 1 <= mn.maxLocals;

        mn.maxLocals = updMax + 1;

        super.transform(mn);
    }

    private boolean isWide(final VarInsnNode vnode) {
        switch (vnode.getOpcode()) {
            case Opcodes.LLOAD:
            case Opcodes.DLOAD:
            case Opcodes.LSTORE:
            case Opcodes.DSTORE:
                return true;
            default:
                return false;
        }
    }

    /**
     *  Remove dangling LocalVarNodes for non-params (ie those LocalVarNodes whose local-var lacks any LOAD or STORE).
     *  Re-number those LocalVarNodes whose variable has been re-numbered.
     * */
    private void compactLocalVarEntries(final MethodNode mn, final int numPLocals, final int[] renumber) {

        if(mn.localVariables != null) {
            Iterator<LocalVariableNode> lvIter = mn.localVariables.iterator();
            while (lvIter.hasNext()) {
                LocalVariableNode lvn = lvIter.next();
                boolean isParam = (lvn.index < numPLocals);
                if(!isParam) {
                    boolean isRedundant = (renumber[lvn.index] == -1);
                    if(isRedundant) {
                        lvIter.remove();
                    } else {
                        boolean shouldRenum = (renumber[lvn.index] != lvn.index);
                        if(shouldRenum) {
                            lvn.index = renumber[lvn.index];
                        }
                    }
                }
            }
        }
    }

}
