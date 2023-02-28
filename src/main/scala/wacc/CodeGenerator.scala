package wacc

import scala.collection.mutable.ListBuffer
import wacc.Registers._
import wacc.FreqCodeBlocks

object CodeGenerator {
    var controlFlowGraph = new InstBlock()
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
        val funcBlock = new FuncBlock()

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
    def translate(node: AssignIdentNode): Unit = {
        node.rvalue match {
            case e: ExprNode => {
                val op = translate(e)
                currInstBlock.addInst(new MovInst(new Variable(node.ident.name), op))
                // This does not account for negative integers, whc shd use ldr
            }
            case ArrayLiterNode(exprList) => {
                // Detect type to determine allocation size for each element
                val allocSize = getOffset(exprList(0))
                var stackOffset = 0
                // Calling malloc to get the address storing the array
                currInstBlock.addInst(FreqCodeBlocks.allocSpc(4 + allocSize * exprList.length))

                // Storing the size of the array on the first 4 bytes / word
                currInstBlock.addInst(MovInst(r8, ImmVal(exprList.length, IntIdentifier())))
                currInstBlock.addInst(StrInst(r8, Offset(r10, stackOffset)))

                // Iterate through the array to insert the elements
                for (expr <- exprList) {
                    stackOffset += allocSize
                    currInstBlock.addInst(MovInst(r8, translate(expr)))
                    currInstBlock.addInst(StrInst(r8, Offset(r10, stackOffset)))
                }

                // Storing the address into register or memory assigned for array
                currInstBlock.addInst(MovInst(Variable(node.ident.name), r8))
            }
            case NewPairNode(fstExpr, sndExpr) => {
                val saveVal = List(
                    StrInst(r8, Offset(r10, 0)),
                    PushInst(List(r10))
                )
                val fstOffset = getOffset(fstExpr)
                val sndOffset = getOffset(sndExpr)
                currInstBlock.addInst(
                    // Alloc for first element
                    FreqCodeBlocks.allocSpc(fstOffset) ++
                    List(MovInst(r8, translate(fstExpr))) ++
                    saveVal ++
                    // Alloc for second element
                    FreqCodeBlocks.allocSpc(sndOffset) ++
                    List(MovInst(r8, translate(sndExpr))) ++
                    saveVal ++
                    // Alloc for pointers pointing to both elements
                    FreqCodeBlocks.allocSpc(8) ++
                    // Popping the addresses from the stack and storing them
                    List(PopInst(List(r8)),
                         StrInst(r8, Offset(r10, 4)),
                         PopInst(List(r8)),
                         StrInst(r8, Offset(r10, 0)),
                         MovInst(Variable(node.ident.name), r10)))
                
            }
            case CallNode(ident, argList) => {
                // Caller-save push r0 - r3 ?????
                currInstBlock.addInst(
                    pushArgs(0, argList.exprList, ListBuffer[Instruction]())
                    .toList)
                currInstBlock.addInst(BranchLinkInst(ident.name))
                currInstBlock.addInst(MovInst(Variable(node.ident.name), r0))
                // Caller-save pop r0 - r3 ?????
            }
            case FstNode(lvalue) => {
                currInstBlock.addInst(accessPairElem(0, lvalue).toList)
                currInstBlock.addInst(MovInst(Variable(node.ident.name), r8))
            }
            case SndNode(lvalue) => {
                currInstBlock.addInst(accessPairElem(4, lvalue).toList)
                currInstBlock.addInst(MovInst(Variable(node.ident.name), r8))
            }
        }
    }

    def getOffset(expr: ExprNode): Int = {
        expr.typeVal() match {
            case CharIdentifier() => 1
            case _ => 4
        }
    }

    def accessPairElem(pairOffset: Int, lvalue: LValueNode): ListBuffer[Instruction] = {
        val insts = ListBuffer[Instruction]()

        lvalue match {
            case IdentNode(name) => {
                insts ++= List(
                    CmpInst(Variable(name), ImmVal(0, IntIdentifier())),
                    BLEqInst("_errNull"),
                    LdrInst(r8, Offset(Variable(name), pairOffset)),
                )
                lvalue.typeVal() match {
                    case CharIdentifier() => insts += LdrsbInst(r8, Offset(r8, 0))
                    case _ => insts += LdrInst(r8, Offset(r8, 0))
                }
            }
            case p: PairElemNode => {
                p match {
                    case FstNode(lvalue) => accessPairElem(0, lvalue)
                    case SndNode(lvalue) => accessPairElem(4, lvalue)
                }
                insts ++= List(
                    CmpInst(r8, ImmVal(0, IntIdentifier())),
                    BLEqInst("_errNull"),
                    LdrInst(r8, Offset(r8, pairOffset)),
                )
                lvalue.typeVal() match {
                    case CharIdentifier() => insts += LdrsbInst(r8, Offset(r8, 0))
                    case _ => insts += LdrInst(r8, Offset(r8, 0))
                }
            }
            case _ => {println("what")} 
        }
        insts
    }

    def pushArgs(regCount: Int, args: List[ExprNode],
        insts: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        if (args.isEmpty) {
            return insts
        } else if (regCount >= 4) {
            return pushStack(args, insts)
        }
        insts += MovInst(FixedRegister(regCount), translate(args(0)))
        pushArgs(regCount + 1, args.drop(1), insts)
        insts
    }

    def pushStack(args: List[ExprNode], insts: ListBuffer[Instruction]): ListBuffer[Instruction] = {
        if (args.isEmpty) {
            return insts
        }
        val argOffset = getOffset(args(0))
        insts += MovInst(r8, translate(args(0)))
        insts += StrInst(r8, Offset(sp, -argOffset))
        pushStack(args.drop(1), insts)
    }

    def translate(node: LValuesAssignNode): Unit = {
        node.lvalue match {
            case IdentNode(name) => {
                val dummyNode = AssignIdentNode(BaseTypeNode("int"),
                    IdentNode(name), node.rvalue)
                translate(dummyNode)
            }
            case ArrayElemNode(ident, exprList) => {
                // Assuming 1d array
                val insts = List(
                    MovInst(r10, translate(exprList(0))),
                    // Need to abstract assignident code blocks for this bit to call
                    //currInstBlock.addInst(MovInst(r8, node.rvalue))
                    MovInst(r3, Variable(ident.name)),
                    BranchLinkInst("_arrStore")
                    //arrstore to be defined
                )
                currInstBlock.addInst(insts)
            }
            case FstNode(lvalue) => ???
            case SndNode(lvalue) => ???
        }
    }
    def translate(node: ReadNode): Unit = {}
    def translate(node: FreeNode): Unit = {}
    def translate(node: ReturnNode): Unit = {
        val op = translate(node.expr)
        val inst = new MovInst(new FixedRegister(0), op)
        currInstBlock.addInst(inst)
    }
    def translate(node: ExitNode): Unit = {
        // currInstBlock.addInst()
        val op = translate(node.expr)
        val inst = new MovInst(new FixedRegister(0), op)
        val inst2 = new BranchLinkInst("exit")
        currInstBlock.addInst(inst)
        currInstBlock.addInst(inst2)
    }
    def translate(node: PrintNode): Unit = {
        /** 
         * TODO: push and pop r0 - r3 before calling
         * print may clobber any registers that are marked as caller-save under
         * arm's calling convention: R0, R1, R2, R3
        */
        val retOp = translate(node.expr)
        val exprTy = node.expr.typeVal()
        val printInstr = new PrintFunc()
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
        val ifBlock = new IfBlock()
        currInstBlock = ifBlock.cond
        // translate cond
        currInstBlock = ifBlock.nextT
        translate(node.fstStat)
        currInstBlock = ifBlock.nextF
        translate(node.sndStat)
        currInstBlock = ifBlock.next
    }
    def translate(node: WhileNode): Unit = {
        val whileBlock = new WhileBlock()
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
        val immTrue = ImmVal(1, BoolIdentifier())
        val immFalse = ImmVal(0, BoolIdentifier())

        def translateComparisonOperators(op1: Operand, op2: Operand, cond: String, notcond: String): Register = {
            var reg =  TempRegister()
            op1 match {
                case ImmVal(num, ty) => {
                    var reg1 = TempRegister()
                    currInstBlock.addInst(MovInst(reg1, op1))
                    currInstBlock.addInst(CmpInst(reg1, op2))
                    currInstBlock.addInst(MovCondInst(cond, reg, immTrue))
                    currInstBlock.addInst(MovCondInst(notcond, reg, immFalse))
                }
                case _ => {
                    currInstBlock.addInst(CmpInst(op1.asInstanceOf[Register], op2))
                    currInstBlock.addInst(MovCondInst(cond, reg, immTrue))
                    currInstBlock.addInst(MovCondInst(notcond, reg, immFalse))
                }
            }
            return reg
        }
        
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
                    case ImmVal(num, ty) => {
                        if (num == 0) op = immTrue
                        else op = immFalse
                        return op 
                    }
                    // TODO: asInstanceOf or put all remaining cases
                    case _ => {
                        var inst1 = CmpInst(op.asInstanceOf[Register], ImmVal(1, expr.typeVal()))
                        var reg = TempRegister()
                        var inst2 = MovCondInst("NE", reg, ImmVal(1, expr.typeVal()))
                        var inst3 = MovCondInst("Eq", reg, ImmVal(0, expr.typeVal()))
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
                    case ImmVal(num, ty) => {
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
                    case ImmVal(num, ty) => {
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
                    case ImmVal(num, ty) => {
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
                currInstBlock.addInst(BranchCondInst("Eq", "_errDivZero"))
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
                currInstBlock.addInst(BranchCondInst("Eq", "_errDivZero"))
                currInstBlock.addInst(BranchLinkInst("__aeabi_idivmod"))
                var reg2 = TempRegister()
                currInstBlock.addInst(MovInst(reg2, reg1))
                return reg2
            }
            case AddNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                op1 match {
                    case ImmVal(num, ty) => {
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
                    case ImmVal(num, ty) => {
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
            case GTNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                return translateComparisonOperators(op1, op2, "GT", "LE")
            }
            case GTENode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                return translateComparisonOperators(op1, op2, "GE", "LT")
            }
            case LTNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                return translateComparisonOperators(op1, op2, "LT", "GE")
            }
            case LTENode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                return translateComparisonOperators(op1, op2, "LE", "GT")
            }
            case EqNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                return translateComparisonOperators(op1, op2, "Eq", "NE")
            }
            case IEqNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                return translateComparisonOperators(op1, op2, "NE", "Eq")
            }
            // TODO: fix label
            case AndNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                var reg = TempRegister()
                var label = Label()
                op1 match {
                    case ImmVal(num, ty) => {
                        var reg1 = TempRegister()
                        currInstBlock.addInst(MovInst(reg1, op1))
                        currInstBlock.addInst(CmpInst(reg1, immTrue))
                        currInstBlock.addInst(BranchCondInst("NE", "label"))
                        op2 match {
                            case ImmVal(num, ty) => {
                                var reg2 = TempRegister()
                                currInstBlock.addInst(MovInst(reg2, op2))
                                currInstBlock.addInst(CmpInst(reg2, immTrue))
                            }
                            case _ => {
                                currInstBlock.addInst(CmpInst(op2.asInstanceOf[Register], immTrue))
                            }
                        }
                    }
                    case _ => {
                       currInstBlock.addInst(CmpInst(op1.asInstanceOf[Register], immTrue)) 
                       currInstBlock.addInst(BranchCondInst("NE", "label"))
                       op2 match {
                            case ImmVal(num, ty) => {
                                var reg2 = TempRegister()
                                currInstBlock.addInst(MovInst(reg2, op2))
                                currInstBlock.addInst(CmpInst(reg2, immTrue))
                            }
                            case _ => {
                                currInstBlock.addInst(CmpInst(op2.asInstanceOf[Register], immTrue))
                            }
                        }
                    }
                }
                currInstBlock.addInst(label)
                currInstBlock.addInst(MovCondInst("Eq", reg, immTrue))
                currInstBlock.addInst(MovCondInst("NE", reg, immFalse))
                return reg
            }
            case OrNode(fstexpr, sndexpr) => {
                var op1 = translate(fstexpr)
                var op2 = translate(sndexpr)
                var reg = TempRegister()
                var label = Label()
                op1 match {
                    case ImmVal(num, ty) => {
                        var reg1 = TempRegister()
                        currInstBlock.addInst(MovInst(reg1, op1))
                        currInstBlock.addInst(CmpInst(reg1, immTrue))
                        currInstBlock.addInst(BranchCondInst("Eq", "label"))
                        op2 match {
                            case ImmVal(num, ty) => {
                                var reg2 = TempRegister()
                                currInstBlock.addInst(MovInst(reg2, op2))
                                currInstBlock.addInst(CmpInst(reg2, immTrue))
                            }
                            case _ => {
                                currInstBlock.addInst(CmpInst(op2.asInstanceOf[Register], immTrue))
                            }
                        }
                    }
                    case _ => {
                       currInstBlock.addInst(CmpInst(op1.asInstanceOf[Register], immTrue)) 
                       currInstBlock.addInst(BranchCondInst("Eq", "label"))
                       op2 match {
                            case ImmVal(num, ty) => {
                                var reg2 = TempRegister()
                                currInstBlock.addInst(MovInst(reg2, op2))
                                currInstBlock.addInst(CmpInst(reg2, immTrue))
                            }
                            case _ => {
                                currInstBlock.addInst(CmpInst(op2.asInstanceOf[Register], immTrue))
                            }
                        }
                    }
                }
                currInstBlock.addInst(label)
                currInstBlock.addInst(MovCondInst("Eq", reg, immTrue))
                currInstBlock.addInst(MovCondInst("NE", reg, immFalse))
                return reg
            }
        }
    }
}
