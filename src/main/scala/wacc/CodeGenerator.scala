package wacc

import scala.collection.mutable.{ListBuffer, Map}
import wacc.Constants._

object CodeGenerator {
    var controlFlowGraph = InstBlock()
    var currInstBlock = controlFlowGraph
    /* NEW: temporory design to accomodate label jumps */
    var controlFlowFuncs = ListBuffer[FuncBlock]()

    def translateAST(p: ProgramNode): Unit = {
        for (func <- p.funcList) {
            translate(func)
        }
        translate(p.stat)
    }

    def translate(f: FuncNode): Unit = {
        val funcBlock = FuncBlock()

        // // Sample: To be corrected/checked
        // val varList: ListBuffer[Variable] = ListBuffer.empty
        // for (p <- f.paramList.paramList) {
        //     regList += new Variable()
        // }
        // funcBlock.param.addInst(PushInst(regList.toList))
        // //

        currInstBlock = funcBlock.body
        translate(f.stat)
    }

    def translate(node: StatNode): Unit = {
        node match {
            case SkipNode() | AssignIdentNode(_,_,_) | LValuesAssignNode(_,_) | ReadNode(_) | 
                 FreeNode(_) | ReturnNode(_) | ExitNode(_) | PrintNode(_) | PrintlnNode(_) | 
                 IfNode(_,_,_) | WhileNode(_,_) | BeginEndNode(_) | StatJoinNode(_) => 
                translate(node)
        }
    }
    def translate(node: SkipNode): Unit = {}
    def translate(node: AssignIdentNode): Unit = {}
    def translate(node: LValuesAssignNode): Unit = {}
    def translate(node: ReadNode): Unit = {}
    def translate(node: FreeNode): Unit = {}
    def translate(node: ReturnNode): Unit = {
        val op = translate(node.expr)
        currInstBlock.addInst(MovInst(Constants.r0, op))
    }
    def translate(node: ExitNode): Unit = {
        // currInstBlock.addInst()
        val op = translate(node.expr)
        currInstBlock.addInst(MovInst(r0, op))
        currInstBlock.addInst(BranchLinkInst("exit"))
    }
    def translate(node: PrintNode): Unit = {
        /** 
         * TODO: push and pop r0 - r3 before calling
         * print may clobber any registers that are marked as caller-save under
         * arm's calling convention: R0, R1, R2, R3
        */
        val retOp = translate(node.expr)
        val exprTy = node.expr.typeVal()
        val printInstr = new IOFunc()
        exprTy match {
            case CharIdentifier() => printInstr.printInt(retOp)
            case IntIdentifier() => ???
            case StrIdentifier() => ???
            case BoolIdentifier() => ???
            case a: ArrayIdentifier => ???
            case p: PairIdentifier => ???
            case _ => throw new IllegalArgumentException("print: not an expression")
        }
    }

    def translate(node: PrintlnNode): Unit = {
        // TODO: add new line
        translate(new PrintNode(node.expr))
    }

    def translate(node: IfNode): Unit = {
        val ifBlock = IfBlock()
        val ifTrue = ifBlock.nextT
        val ifFalse = ifBlock.nextF
        val next = ifBlock.next
        val op = translate(node.expr)
        op match {
            case ImmVal(num, ty) => {
                currInstBlock.addInst(MovInst(r8, op))
                currInstBlock.addInst(CmpInst(r8, immTrue))
            }
            case r: Register => {
                currInstBlock.addInst(CmpInst(r, immTrue))
            }
        }
        currInstBlock.addInst(BranchNumCondInst("NE", ifFalse.num))
        currInstBlock = ifTrue
        translate(node.fstStat)
        currInstBlock.addInst(BranchNumInst(next.num))
        currInstBlock = ifFalse
        translate(node.sndStat)
        currInstBlock = next
    }
    def translate(node: WhileNode): Unit = {
        val whileBlock = WhileBlock()
        val cond = whileBlock.cond
        val loop = whileBlock.loop
        val next = whileBlock.next
        currInstBlock = cond
        translate(node.expr)
        currInstBlock.addInst(BranchNumCondInst("NE", next.num))
        currInstBlock = loop
        translate(node.stat)
        currInstBlock.addInst(BranchNumInst(cond.num))
        currInstBlock = next
    }
    def translate(node: BeginEndNode): Unit = {
        translate(node.stat)
    }
    def translate(node: StatJoinNode): Unit = {
        for (n <- node.statList) {
            translate(n)
        }
    }

    def translate(node: ExprNode): Operand = {
        def translateComparisonOperators(op1: Register, op2: Operand, cond: String, notcond: String): Register = {
            currInstBlock.addInst(CmpInst(op1, op2))
            currInstBlock.addInst(MovCondInst(cond, r8, immTrue))
            currInstBlock.addInst(MovCondInst(notcond, r8, immFalse))
            return r8
        }
        
        node match {
            case IntLiterNode(n) => {
                if (n >= 0) {return ImmVal(n, node.typeVal())}
                else {
                    currInstBlock.addInst(LdrPseudoInst(r8, n))
                    return r8
                }
            }
            case BoolLiterNode(true) => {
                return ImmVal(1, node.typeVal())
            }
            case BoolLiterNode(false) => {
                return ImmVal(0, node.typeVal())
            }
            case CharLiterNode(c) => {
                val num = c.toInt
                return ImmVal(num, node.typeVal())
            }
            case StrLiterNode(s) => {
                ???
            }            
            case PairLiterNode() => {
                return ImmVal(0, node.typeVal())
            }
            case n: IdentNode => {
                return Variable(n.newName)
            }
            case ArrayElemNode(ident, exprList) => {
                val op1 = translate(ident)
                // currInstBlock.addInst(PushInst(List(r3)))
                currInstBlock.addInst(MovInst(r8, op1))
                for (expr <- exprList) {
                    currInstBlock.addInst(PushInst(List(r8)))
                    val op2 = translate(expr)
                    currInstBlock.addInst(MovInst(r10, op2))
                    currInstBlock.addInst(PopInst(List(r8)))
                    currInstBlock.addInst(BranchLinkInst("_arrLoad"))
                }
                return r8
            }
            case NotNode(expr) => {
                var op = translate(expr)
                op match {
                    case ImmVal(num, ty) => {
                        if (num == 0) op = immTrue
                        else op = immFalse
                        return op 
                    }
                    // TODO: asInstanceOf or put all remaining cases
                    case r: Register => {
                        currInstBlock.addInst(CmpInst(r, ImmVal(1, expr.typeVal())))
                        currInstBlock.addInst(MovCondInst("NE", r8, ImmVal(1, expr.typeVal())))
                        currInstBlock.addInst(MovCondInst("Eq", r8, ImmVal(0, expr.typeVal())))
                        return r8 
                    }
                }            
            }
            case NegNode(expr) => {
                val op = translate(expr)
                op match {
                    case ImmVal(num, ty) => {
                        currInstBlock.addInst(MovInst(r8, op))
                        currInstBlock.addInst(NegInst(r8, r8))
                    }
                    case r: Register => {
                        currInstBlock.addInst(NegInst(r8, r))
                    }
                }
                return r8
            }
            case LenNode(expr) => {
                val op = translate(expr)
                val offset = Offset(op.asInstanceOf[Register], arrLenOffset)
                currInstBlock.addInst(LdrInst(r8, offset))
                return r8
            }
            case OrdNode(expr) => {
                val op = translate(expr)
                op match {
                    case ImmVal(num, ty) => {
                        return ImmVal(num, IntIdentifier())
                    }
                    case _: Register => {
                        return op
                    }
                }
            }
            case ChrNode(expr) => {
                val op = translate(expr)
                op match {
                    case ImmVal(num, ty) => {
                        return ImmVal(num, CharIdentifier())
                    }
                    case _: Register => {
                        return op
                    }
                }
            }
            case MulNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister()
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                var reg2: Register = r9
                op2 match {
                    case ImmVal(num, ty) => {
                    currInstBlock.addInst(MovInst(reg2, op2))
                    }
                    case r: Register => {
                        reg2 = r
                    }
                }
                currInstBlock.addInst(SmullInst(r8, r9, reg1, reg2))
                currInstBlock.addInst(CmpInst(r9, ASR(r8, 31)))
                currInstBlock.addInst(BranchCondInst("NE", "_errOverflow"))
                return r8
            }
            case DivNode(fstexpr, sndexpr) => {
                currInstBlock.addInst(PushInst(List(r0, r1)))
                val op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(r0, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(MovInst(r1, op2))
                currInstBlock.addInst(CmpInst(r1, ImmVal(0, IntIdentifier())))
                currInstBlock.addInst(BranchCondInst("Eq", "_errDivZero"))
                currInstBlock.addInst(BranchLinkInst("__aeabi_idivmod"))
                currInstBlock.addInst(MovInst(r8, r0))
                currInstBlock.addInst(PopInst(List(r0, r1)))
                return r8
            }
            case ModNode(fstexpr, sndexpr) => {
                currInstBlock.addInst(PushInst(List(r0, r1)))
                val op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(r0, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(MovInst(r1, op2))
                currInstBlock.addInst(CmpInst(r1, ImmVal(0, IntIdentifier())))
                currInstBlock.addInst(BranchCondInst("Eq", "_errDivZero"))
                currInstBlock.addInst(BranchLinkInst("__aeabi_idivmod"))
                currInstBlock.addInst(MovInst(r8, r1))
                currInstBlock.addInst(PopInst(List(r0, r1)))
                return r8
            }
            case AddNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                op1 match {
                    case ImmVal(num, ty) => {
                        val reg = TempRegister()
                        currInstBlock.addInst(MovInst(reg, op1))
                        val op2 = translate(sndexpr)
                        currInstBlock.addInst(AddsInst(r8, reg, op2))
                    }
                    case r: Register => {
                        val op2 = translate(sndexpr)
                        currInstBlock.addInst(AddsInst(r8, r, op2))
                    }
                }
                return r8
            }
            case SubNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                op1 match {
                    case ImmVal(num, ty) => {
                        val reg = TempRegister()
                        currInstBlock.addInst(MovInst(reg, op1))
                        val op2 = translate(sndexpr)
                        currInstBlock.addInst(SubsInst(r8, reg, op2))
                    }
                    case r: Register => {
                        val op2 = translate(sndexpr)
                        currInstBlock.addInst(SubsInst(r8, r, op2))
                    }
                }
                return r8
            }
            case GTNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                return translateComparisonOperators(reg, op2, "GT", "LE")
            }
            case GTENode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                return translateComparisonOperators(reg, op2, "GE", "LT")
            }
            case LTNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                return translateComparisonOperators(reg, op2, "LT", "GE")
            }
            case LTENode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                return translateComparisonOperators(reg, op2, "LE", "GT")
            }
            case EqNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                return translateComparisonOperators(reg, op2, "Eq", "NE")
            }
            case IEqNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                return translateComparisonOperators(reg, op2, "NE", "Eq")
            }
            // TODO: fix label
            case AndNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister()
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                var reg2: Register = reg1
                op2 match {
                    case ImmVal(num, ty) => {
                        reg2 = TempRegister()
                        currInstBlock.addInst(MovInst(reg2, op2))
                    }
                    case r: Register => {
                        reg2 = r
                    }
                }
                val newBlock = InstBlock()
                currInstBlock.addInst(CmpInst(reg1, immTrue)) 
                currInstBlock.addInst(BranchNumCondInst("NE", newBlock.num))
                currInstBlock.addInst(CmpInst(reg2, immTrue))
                currInstBlock = newBlock
                currInstBlock.addInst(MovCondInst("Eq", r8, immTrue))
                currInstBlock.addInst(MovCondInst("NE", r8, immFalse))
                return r8
            }
            case OrNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister()
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                var reg2: Register = reg1
                op2 match {
                    case ImmVal(num, ty) => {
                        reg2 = TempRegister()
                        currInstBlock.addInst(MovInst(reg2, op2))
                    }
                    case r: Register => {
                        reg2 = r
                    }
                }
                val newBlock = InstBlock()
                currInstBlock.addInst(CmpInst(reg1, immTrue)) 
                currInstBlock.addInst(BranchNumCondInst("Eq", newBlock.num))
                currInstBlock.addInst(CmpInst(reg2, immTrue))
                currInstBlock = newBlock
                currInstBlock.addInst(MovCondInst("Eq", r8, immTrue))
                currInstBlock.addInst(MovCondInst("NE", r8, immFalse))
                return r8
            }
        }
    }
}
