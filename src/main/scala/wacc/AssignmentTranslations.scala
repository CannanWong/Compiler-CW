package wacc

import scala.collection.mutable.ListBuffer
import wacc.CodeGenerator._
import wacc.Registers._

object AssignmentTranslations {
    // General function for translating RHS Values
    def transRVal(rvalue: RValueNode): Operand = {
        rvalue match {
            case ArrayLiterNode(exprList) => transArray(exprList)
            case NewPairNode(fstExpr, sndExpr) => transNewPair(fstExpr, sndExpr)
            case CallNode(ident, argList) => transCall(ident, argList)
            case FstNode(lvalue) => accessPairElem(0, lvalue)
            case SndNode(lvalue) => accessPairElem(4, lvalue)
            case e: ExprNode => translate(e)
            case _ => {throw new UnsupportedOperationException("what")}
        }
    }   
    def transArray(exprList: List[ExprNode]): Operand = {
        val exprs = exprList

        // Detect type to determine allocation size for each element
        val allocSize = getOffset(exprs(0))
        var stackOffset = 0

        // Calling malloc to get the address storing the array
        currInstBlock.addInst(allocSpc(4 + allocSize * exprs.length))

        // Storing the size of the array on the first 4 bytes / word
        currInstBlock.addInst(MovInst(r8, ImmVal(exprs.length)))
        currInstBlock.addInst(StrInst(r8, Offset(r10, stackOffset)))    

        // Iterate through the array to insert the elements
        for (expr <- exprs) {
            stackOffset += allocSize
            currInstBlock.addInst(MovInst(r8, translate(expr)))
            currInstBlock.addInst(StrInst(r8, Offset(r10, stackOffset)))
        }
        currInstBlock.addInst(MovInst(r8, r10))
        r8
    }   
    def transNewPair(fstExpr: ExprNode, sndExpr: ExprNode): Operand = {
        val saveVal = List(
            StrInst(r8, Offset(r10, 0)),
            PushInst(r10)
        )
        val fstOffset = getOffset(fstExpr)
        val sndOffset = getOffset(sndExpr)
        currInstBlock.addInst(
            // Alloc for first element
            allocSpc(fstOffset) ++
            List(MovInst(r8, translate(fstExpr))) ++
            saveVal ++

            // Alloc for second element
            allocSpc(sndOffset) ++
            List(MovInst(r8, translate(sndExpr))) ++
            saveVal ++

            // Alloc for pointers pointing to both elements
            allocSpc(8) ++
            
            // Popping the addresses from the stack and storing them
            List(PopInst(r8),
                 StrInst(r8, Offset(r10, 4)),
                 PopInst(r8),
                 StrInst(r8, Offset(r10, 0)),
                 MovInst(r8, r10)))
            r8
    }   
    def transCall(ident: IdentNode, argList: ArgListNode): Operand = {
        // Push Caller Regs
        currInstBlock.addInst(PushInst(r0, r1, r2, r3))
        // Push Args onto r0, 1, 2, 3, and stack afterwards
        currInstBlock.addInst(
            pushArgs(0, argList.exprList, ListBuffer[Instruction]())
            .toList)
        // Branch link to the function
        currInstBlock.addInst(BranchLinkInst(ident.name))
        r0
    }


    def accessPairElem(pairOffset: Int, lvalue: LValueNode): Operand = {
        var pos: Register = r8
        lvalue match {
            case IdentNode(name) => pos = Variable(name)
            case ArrayElemNode(ident, exprList) => translate(ArrayElemNode(ident, exprList))
            case FstNode(lvalue) => accessPairElem(0, lvalue)
            case SndNode(lvalue) => accessPairElem(4, lvalue)
        }   
        currInstBlock.addInst(
            CmpInst(pos, ImmVal(0)),
            BranchLinkCondInst("Eq", "_errNull"),
            LdrInst(r8, Offset(pos, pairOffset))
        )
        lvalue.typeVal() match {
            case CharIdentifier() => currInstBlock.addInst(LdrsbInst(r8, Offset(r8, 0)))
            case _ => currInstBlock.addInst(LdrInst(r8, Offset(r8, 0)))
        }
        r8
    }   
    def storeToPairElemAddr(pairOffset: Int, lvalue: LValueNode, op: Operand): Operand = {
        var pos: Register = r8
        lvalue match {
            case IdentNode(name) => pos = Variable(name)
            case ArrayElemNode(ident, exprList) => translate(ArrayElemNode(ident, exprList))
            case FstNode(lvalue) => accessPairElem(0, lvalue)
            case SndNode(lvalue) => accessPairElem(4, lvalue)
        }
        currInstBlock.addInst(
            CmpInst(pos, ImmVal(0)),
            BranchLinkCondInst("Eq", "_errNull"),
            LdrInst(r8, Offset(pos, 0)),
            MovInst(r9, r8),
            MovInst(r8, op),
            StrInst(r8, Offset(r9, 0))
        )
        r8
    }   
    def getOffset(expr: ExprNode): Int = {
        expr.typeVal() match {
            case CharIdentifier() => 1
            case _ => 4
        }
    }   
    def pushArgs(regCount: Int, args: List[ExprNode],
        insts: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        if (args.isEmpty) {
            return insts
        } else if (regCount >= 4) {
            return pushStack(args, insts)
        }
        insts += MovInst(FixedRegister(regCount), translate(args(0)))
        if (args.drop(1).isEmpty) insts
        else if (regCount >= 4) pushStack(args, insts)
        else pushArgs(regCount + 1, args, insts)
    }   
    def pushStack(args: List[ExprNode], insts: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        val argOffset = getOffset(args(0))
        insts += MovInst(r8, translate(args(0)))
        insts += StrInst(r8, Offset(sp, -argOffset))
        if (args.drop(1).isEmpty) insts else pushStack(args, insts)
    }

    def allocSpc(spc: Int): List[Instruction] = { List(
        MovInst(r0, ImmVal(spc)),
        BranchLinkInst("malloc"),
        MovInst(r10, r0))
    }
}
