package wacc

import scala.collection.mutable.ListBuffer

object CodeGenerator {
    var controlFlowGraph = InstBlock()
    var currInstBlock = controlFlowGraph

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
        var op = translate(node.expr)
        var inst = MovInst(FixedRegister(0), op)
        currInstBlock.addInst(inst)
    }
    def translate(node: ExitNode): Unit = {
        // currInstBlock.addInst()
        var op = translate(node.expr)
        var inst1 = MovInst(FixedRegister(0), op)
        var inst2 = BranchLinkInst("exit")
        currInstBlock.addInst(inst1)
        currInstBlock.addInst(inst2)
    }
    def translate(node: PrintNode): Unit = {}
    def translate(node: PrintlnNode): Unit = {}
    def translate(node: IfNode): Unit = {
        val ifBlock = IfBlock()
        currInstBlock = ifBlock.cond
        // translate cond
        currInstBlock = ifBlock.nextT
        translate(node.fstStat)
        currInstBlock = ifBlock.nextF
        translate(node.sndStat)
        currInstBlock = ifBlock.next
    }
    def translate(node: WhileNode): Unit = {
        val whileBlock = WhileBlock()
        currInstBlock = whileBlock.cond
        // translate cond
        currInstBlock = whileBlock.loop
        translate(node.stat)
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
        node match {
            case IntLiterNode(n) => {
                if (n >= 0) {return ImmVal(n, node.typeVal())}
                else {
                    var reg = TempRegister()
                    var inst = LdrPseudoInst(reg, n)
                    currInstBlock.addInst(inst)
                    return reg
                }
            }
            case BoolLiterNode(true) => {
                return ImmVal(1, node.typeVal())
            }
            case BoolLiterNode(false) => {
                return ImmVal(0, node.typeVal())
            }
            case CharLiterNode(c) => {
                var num = c.toInt
                return ImmVal(num, node.typeVal())
            }
            case StrLiterNode(s) => {
                ???
            }            
            case PairLiterNode() => {
                return ImmVal(0, node.typeVal())
            }
            case NotNode(expr) => {
                var op = translate(expr)
                op match {
                    case ImmVal(num, t) => {
                        if (num == 0) op = ImmVal(1, t)
                        else op = ImmVal(0, t)
                        return op 
                    }
                    // TODO: asInstanceOf or put all remaining cases
                    case _ => {
                        var inst1 = CmpInst(op.asInstanceOf[Register], ImmVal(1, expr.typeVal()))
                        var reg = TempRegister()
                        var inst2 = MovNEqInst(reg, ImmVal(1, expr.typeVal()))
                        var inst3 = MovEqInst(reg, ImmVal(0, expr.typeVal()))
                        currInstBlock.addInst(List(inst1, inst2, inst3))
                        // var inst4 = MovInst(op, reg)
                        return reg 
                    }
                }            
            }
            case NegNode(expr) => {
                var op = translate(expr)
                var reg = TempRegister()
                op match {
                    case ImmVal(num, t) => {
                        currInstBlock.addInst(MovInst(reg, op))
                        currInstBlock.addInst(NegInst(reg, reg))
                    }
                    case _ => {
                        var inst = NegInst(reg, op.asInstanceOf[Register])
                        currInstBlock.addInst(inst)
                    }
                }
                return reg
            }
            case LenNode(expr) => {
                ???
            }
            case OrdNode(expr) => {
                var op = translate(expr)
                op match {
                    case ImmVal(num, t) => {
                        return ImmVal(num, IntIdentifier())
                    }
                    case _ => {
                        var reg = TempRegister()
                        var inst = MovInst(reg, op)
                        currInstBlock.addInst(inst)
                        return reg
                    }
                }
            }
            case ChrNode(expr) => {
                var op = translate(expr)
                op match {
                    case ImmVal(num, t) => {
                        return ImmVal(num, CharIdentifier())
                    }
                    case _ => {
                        var reg = TempRegister()
                        var inst = MovInst(reg, op)
                        currInstBlock.addInst(inst)
                        return reg
                    }
                }
            }
            case MulNode(fstexpr, sndexpr) => {
                var reg1 = TempRegister()
                var reg2 = TempRegister()
                var op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(reg1, op1))
                var op2 = translate(sndexpr)
                currInstBlock.addInst(MovInst(reg2, op2))
                var reg3 = TempRegister()
                var reg4 = TempRegister()
                currInstBlock.addInst(SmullInst(reg3, reg4, reg1, reg2))
                currInstBlock.addInst(CmpInst(reg4, ASR(reg3, 31)))
                currInstBlock.addInst(BranchCondInst("NE", "errOverflow"))
                return reg3
            }
            case DivNode(fstexpr, sndexpr) => {
                var reg0 = FixedRegister(0)
                var reg1 = FixedRegister(1)
                var op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(reg0, op1))
                var op2 = translate(sndexpr)
                currInstBlock.addInst(MovInst(reg1, op2))
                currInstBlock.addInst(CmpInst(reg1, ImmVal(0, IntIdentifier())))
                currInstBlock.addInst(BranchCondInst("EQ", "_errDivZero"))
                currInstBlock.addInst(BranchLinkInst("__aeabi_idivmod"))
                var reg2 = TempRegister()
                currInstBlock.addInst(MovInst(reg2, reg0))
                return reg2
            }
            case ModNode(fstexpr, sndexpr) => {
                var reg0 = FixedRegister(0)
                var reg1 = FixedRegister(1)
                var op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(reg0, op1))
                var op2 = translate(sndexpr)
                currInstBlock.addInst(MovInst(reg1, op2))
                currInstBlock.addInst(CmpInst(reg1, ImmVal(0, IntIdentifier())))
                currInstBlock.addInst(BranchCondInst("EQ", "_errDivZero"))
                currInstBlock.addInst(BranchLinkInst("__aeabi_idivmod"))
                var reg2 = TempRegister()
                currInstBlock.addInst(MovInst(reg2, reg1))
                return reg2
            }
            case AddNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                op1 match {
                    case ImmVal(num, t) => {
                        var reg1 = TempRegister()
                        currInstBlock.addInst(MovInst(reg1, op2))
                        var reg2 = TempRegister()
                        currInstBlock.addInst(AddsInst(reg2, reg1, op1))
                        return reg2
                    }
                    case _ => {
                        var reg = TempRegister()
                        currInstBlock.addInst(AddsInst(reg, op1.asInstanceOf[Register], op2))
                        return reg
                    }
                }
            }
            case SubNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                op1 match {
                    case ImmVal(num, t) => {
                        var reg1 = TempRegister()
                        currInstBlock.addInst(MovInst(reg1, op1))
                        var reg2 = TempRegister()
                        currInstBlock.addInst(SubsInst(reg2, reg1, op2))
                        return reg2
                    }
                    case _ => {
                        var reg = TempRegister()
                        currInstBlock.addInst(SubsInst(reg, op1.asInstanceOf[Register], op2))
                        return reg
                    }
                }
            }
            // case AddNode(fstexpr, sndexpr) => {
                // val rd = Register(1)
                // val rn = Register(2)
                // val op = ImmVal(1)
                // val inst = AddInst(rd, rn, op)
                // currBlock.addInst(inst)
            //}
        }
    }
}
