package wacc

import scala.collection.mutable.ListBuffer
import wacc.CodeGenerator._
import wacc.StandardFuncs._
import wacc.Constants._
import wacc.Constants.StdFuncsEnum._

object AssignmentTranslations {
    // General function for translating RHS Values
    def transRVal(rvalue: RValueNode): Operand = {
        rvalue match {
            case e: ExprNode => translate(e)
            case ArrayLiterNode(exprList) => transNewArray(exprList)
            case NewPairNode(fstExpr, sndExpr) => transNewPair(fstExpr, sndExpr)
            case CallNode(ident, argList) => transCall(ident, argList)
            case FstNode(lvalue) => accessPairElem(0, lvalue)
            case SndNode(lvalue) => accessPairElem(4, lvalue)
            case _ => {throw new UnsupportedOperationException("what")}
        }
    }

    def transNewArray(exprList: List[ExprNode]): Operand = {
        val exprs = exprList
        var stackOffset = -4

        // Detect type to determine allocation size for each element
        var allocSize = 4
        if (!exprList.isEmpty) {
        allocSize = getOffset(exprs(0))
        }

        // Calling malloc to get the address storing the array
        currInstBlock.addInst(
            MovInst(r0, ImmVal(4 + allocSize * exprs.length)),
            BranchLinkInst("malloc"),
            MovInst(r10, r0),

        // Storing the size of the array on the first 4 bytes / word
            AddInst(r10, r10, ImmVal(4)),
            MovInst(r8, ImmVal(exprs.length)),
            StrInst(r8, ImmOffset(r10, stackOffset)))
            
        stackOffset += 4

        // Iterate through the array to insert the elements
        for (expr <- exprs) {
            currInstBlock.addInst(MovInst(r8, translate(expr)))
            if (allocSize == 1) {
                currInstBlock.addInst(StrbInst(r8, ImmOffset(r10, stackOffset)))
            } else {
                currInstBlock.addInst(StrInst(r8, ImmOffset(r10, stackOffset)))
            }
            stackOffset += allocSize
        }
        currInstBlock.addInst(MovInst(r8, r10))
        r8
    }
    
    def transNewPair(fstExpr: ExprNode, sndExpr: ExprNode): Operand = {
        val fstOffset = getOffset(fstExpr)
        val sndOffset = getOffset(sndExpr)
        currInstBlock.addInst(
            // Alloc for first element
            MovInst(r0, ImmVal(fstOffset)),
            BranchLinkInst("malloc"),
            MovInst(r10, r0),
            MovInst(r8, translate(fstExpr)),
            saveVal(fstOffset),
            PushInst(r10),

            MovInst(r0, ImmVal(sndOffset)),
            BranchLinkInst("malloc"),
            MovInst(r10, r0),
            MovInst(r8, translate(sndExpr)),
            saveVal(sndOffset),
            PushInst(r10),

            // Alloc for pointers pointing to both elements
            MovInst(r0, ImmVal(8)),
            BranchLinkInst("malloc"),
            MovInst(r10, r0),
            
            // Popping the addresses from the stack and storing them
            PopInst(r8),
            StrInst(r8, ImmOffset(r10, 4)),
            PopInst(r8),
            StrInst(r8, ImmOffset(r10, 0)),
            MovInst(r8, r10))
        r8
    }

    def getOffset(expr: ExprNode): Int = {
        expr.typeVal() match {
            case CharIdentifier() => 1
            case _ => 4
        }
    }

    def saveVal(offset: Int): Instruction = {
        offset match {
            case 1 => StrbInst(r8, ImmOffset(r10, 0))
            case 4 => StrInst(r8, ImmOffset(r10, 0))
            case _ => throw new UnsupportedOperationException("wrong offset")
        }
    }

    def transCall(ident: IdentNode, argList: ArgListNode): Operand = {
        // Push Caller Regs
        currInstBlock.addInst(PushInst(r0, r1, r2, r3))
        // Push Args onto r0, 1, 2, 3, and stack afterwards
        currInstBlock.addInst(
            pushArgs(0, argList.exprList, ListBuffer[Instruction]())
            .toList)
        // Branch link to the function
        currInstBlock.addInst(BranchLinkInst(s"wacc_${ident.name}"))
        r0
    }

    def accessPairElem(pairOffset: Int, lvalue: LValueNode): Operand = {
        var pos: Register = r8
        lvalue match {
            case i: IdentNode => pos = Variable(i.newName)
            case ArrayElemNode(ident, exprList) => 
                translate(ArrayElemNode(ident, exprList))
            case FstNode(lvalue) => accessPairElem(0, lvalue)
            case SndNode(lvalue) => accessPairElem(4, lvalue)
        }   
        currInstBlock.addInst(
            CmpInst(pos, ImmVal(0)),
            BranchLinkCondInst("Eq", "_errNull"),
            LdrInst(r8, ImmOffset(pos, pairOffset)),
            lvalue.typeVal() match {
                case CharIdentifier() => LdrsbInst(r8, ImmOffset(r8, 0))
                case _ => LdrInst(r8, ImmOffset(r8, 0))
            }
        )
        setUsed(NullErr)
        r8
    }

    def storeToPairElemAddr(pairOffset: Int, lvalue: LValueNode, op: Operand): Operand = {
        var pos: Register = r8
        lvalue match {
            case i: IdentNode => pos = Variable(i.newName)
            case ArrayElemNode(ident, exprList) => 
                translate(ArrayElemNode(ident, exprList))
            case FstNode(lvalue) => accessPairElem(0, lvalue)
            case SndNode(lvalue) => accessPairElem(4, lvalue)
        }
        currInstBlock.addInst(
            CmpInst(pos, ImmVal(0)),
            BranchLinkCondInst("Eq", "_errNull"),
            LdrInst(r8, ImmOffset(pos, pairOffset)),
            MovInst(r9, r8),
            MovInst(r8, op),
            lvalue.typeVal() match {
                case CharIdentifier() => StrbInst(r8, ImmOffset(r9, 0))
                case _ => StrInst(r8, ImmOffset(r9, 0))
            }
        )
        setUsed(NullErr)
        r8
    }

    def pushArgs(regCount: Int, args: List[ExprNode],
        insts: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        if (args.isEmpty) {
            return insts
        }
        insts += MovInst(FixedRegister(regCount), translate(args(0)))
        if (args.drop(1).isEmpty) insts
        else if (regCount >= 4) pushStack(args, insts)
        else pushArgs(regCount + 1, args, insts)
    }

    def pushStack(args: List[ExprNode], insts: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        insts += MovInst(r8, translate(args.head))
        insts += StrChgInst(r8, ImmOffset(sp, -4))
        if (args.drop(1).isEmpty) insts else pushStack(args, insts)
    }
}
