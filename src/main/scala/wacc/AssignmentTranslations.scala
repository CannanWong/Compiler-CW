package wacc

import scala.collection.mutable.ListBuffer
import wacc.CodeGenerator._
import wacc.StandardFuncs._
import wacc.Constants._
import wacc.Constants.StdFuncsEnum._

object AssignmentTranslations {
    var tmpRegs: ListBuffer[TempRegister] = ListBuffer()

    // General function for translating RHS Values
    def transRVal(rvalue: RValueNode): Operand = {
        rvalue match {
            case e: ExprNode => translate(e)
            case ArrayLiterNode(exprList) => transNewArray(exprList)
            case NewPairNode(fstExpr, sndExpr) => transNewPair(fstExpr, sndExpr)
            case CallNode(ident, argList) => transCall(ident, argList)
            case FstNode(lvalue) => accessPairElem(0, lvalue)
            case SndNode(lvalue) => accessPairElem(4, lvalue)
            case _ => 
                {throw new UnsupportedOperationException("operation is not a assignable expression")}
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
            BranchInst("malloc", link=true),
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
            BranchInst("malloc", link=true),
            MovInst(r10, r0),
            MovInst(r8, translate(fstExpr)),
            saveVal(fstOffset),
            PushInst(r10),

            MovInst(r0, ImmVal(sndOffset)),
            BranchInst("malloc", link=true),
            MovInst(r10, r0),
            MovInst(r8, translate(sndExpr)),
            saveVal(sndOffset),
            PushInst(r10),

            // Alloc for pointers pointing to both elements
            MovInst(r0, ImmVal(8)),
            BranchInst("malloc", link=true),
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
        val exprTy = expr match {
            case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get
            case a: ArrayElemNode => SemanticChecker.symbolTable.lookUpVarNewName(a.ident.newName).get
            case _ => expr.typeVal()
        }
        exprTy match {
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
        tmpRegs = ListBuffer()
        // Push Caller Regs
        if (argList.exprList.length >= 4) {
            currInstBlock.addInst(PushInst(r0, r1, r2, r3))
        } else {
            for (c <- 0 to argList.exprList.length - 1) {
                currInstBlock.addInst(PushInst(FixedRegister(c)))
            }
        }
        // Push Args onto r0, 1, 2, 3, and stack afterwards
        if (!argList.exprList.isEmpty) {
            pushArgs(0, argList.exprList)
        }
        var count = 0
        for (tmp <- tmpRegs) {
            currInstBlock.addInst(MovInst(FixedRegister(count), tmp))
            count += 1
        }
        // Branch link to the function
        currInstBlock.addInst(BranchInst(ident.newName, link=true))
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
            BranchInst(NULL_POINTER_LABEL, link=true, condition=Equal()),
            LdrInst(r8, ImmOffset(pos, pairOffset))
        )
        // Find type of lvalue
        val exprTy = lvalue match {
            case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName)
            case a: ArrayElemNode => SemanticChecker.symbolTable.lookUpVarNewName(a.ident.newName)
            case f: FstNode => {
                f.lvalue match {
                    case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName)
                    case _ => f.lvalue.typeVal()
                }
            }
            case s: SndNode => {
                s.lvalue match {
                    case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get
                    case _ => s.lvalue.typeVal()
                }
            }
        }
        currInstBlock.addInst(
            exprTy match {
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
            BranchInst(NULL_POINTER_LABEL, link=true, condition=Equal()),
            LdrInst(r8, ImmOffset(pos, pairOffset)),
            MovInst(r9, r8),
            MovInst(r8, op)
        )
        // Find type of lvalue
        val exprTy = lvalue match {
            case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName)
            case a: ArrayElemNode => SemanticChecker.symbolTable.lookUpVarNewName(a.ident.newName)
            case f: FstNode => {
                f.lvalue match {
                    case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName)
                    case _ => f.lvalue.typeVal()
                }
            }
            case s: SndNode => {
                s.lvalue match {
                    case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName)
                    case _ => s.lvalue.typeVal()
                }
            }
        }
        currInstBlock.addInst(
            exprTy match {
                case CharIdentifier() => StrbInst(r8, ImmOffset(r9, 0))
                case _ => StrInst(r8, ImmOffset(r9, 0))
            }
        )
        setUsed(NullErr)
        r8
    }
    
    // push args onto the stack in order
    def pushArgs(regCount: Int, args: List[ExprNode]): Unit = {
        val tmpReg = TempRegister()

        currInstBlock.addInst(MovInst(tmpReg, translate(args(0))))
        tmpRegs.addOne(tmpReg)
        
        val nextArgs = args.drop(1)
        if (nextArgs.isEmpty) {
            return
        } else if (regCount >= 3) {
            pushStack(nextArgs)
        } else {
            pushArgs(regCount + 1, nextArgs)
        }
    }

    def pushStack(args: List[ExprNode]): Unit = {
        currInstBlock.addInst(
            MovInst(r8, translate(args.head)),
            StrChgInst(r8, ImmOffset(sp, -4))
        )
        val nextArgs = args.drop(1)
        if (nextArgs.isEmpty) return else pushStack(nextArgs)
    }
}
