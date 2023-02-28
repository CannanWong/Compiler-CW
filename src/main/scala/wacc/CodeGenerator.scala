package wacc

import scala.collection.mutable.{ListBuffer, LinkedHashMap}
import wacc.Constants._

object CodeGenerator {
    /* .data directive stores all string declarations */
    var mainFunc = FuncBlock()

    var controlFlowGraph = mainFunc
    
    var currInstBlock = controlFlowGraph.body
    /* NEW: temporory design to accomodate label jumps */
    val controlFlowFuncs = LinkedHashMap[String, ControlFlowBlock]()

    def stringDef(string: String): String = {
        mainFunc.directive.addTextLabelToData(string)
    }

    def translateAST(p: ProgramNode): Unit = {
        for (func <- p.funcList) {
            translate(func)
        }
        translate(p.stat)
    }

    def translate(f: FuncNode): Unit = {
        val funcBlock = FuncBlock()
        f match {
            case FuncNode(ty, ident, param, stat) => {
                /**
                  * TODO:
                    1. set up frame pointer and lr
                    2. get arg from stack / callee saved reg
                    2. 
                  */
                funcBlock.body.addInst(new PushInst(fp, lr))
                /* TODO: push caller saved registers that will be used */
                funcBlock.body.addInst(new MovInst(fp, sp))
                /* TODO: assign args to callee saved register and stack pos */
                translate(stat)
                funcBlock.body.addInst(new MovInst(sp, fp))
                /* TODO: pop caller saved registers that are pushed */
                funcBlock.body.addInst(new PopInst(fp, pc))

                controlFlowFuncs.addOne(s"wacc_${ident.name}", funcBlock)
            }
            case _ => throw new IllegalArgumentException("FuncNode translation receives non FuncNode")
        }

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
    def translate(node: ReadNode): Unit = {
        val retOp = new TempRegister()
        val exprTy = node.lvalue.typeVal()
        exprTy match {
            case CharIdentifier() => IOFunc.readChar(retOp)
            case IntIdentifier() => IOFunc.readInt(retOp)
            case _ => throw new IllegalArgumentException("print: not an int or char")
        }
    }
    def translate(node: FreeNode): Unit = {}
    def translate(node: ReturnNode): Unit = {
        val op = translate(node.expr)
        currInstBlock.addInst(MovInst(Constants.r0, op))
    }
    def translate(node: ExitNode): Unit = {
        val op = translate(node.expr)
        currInstBlock.addInst(
            MovInst(r0, op),
            BranchLinkInst("exit")
        )
    }
    def translate(node: PrintNode): Unit = {
        /** 
         * TODO: push r0 - r3 before calling print in  func call, where r0 -r3 may be storing args
         * pop when return back to scope
         * print may clobber any registers that are marked as caller-save under
         * arm's calling convention: R0, R1, R2, R3
        */
        val retOp = translate(node.expr)
        val exprTy = node.expr.typeVal()
        exprTy match {
            case CharIdentifier() => IOFunc.printChar(retOp)
            case IntIdentifier() => IOFunc.printInt(retOp)
            case StrIdentifier() => IOFunc.printString(retOp)
            case BoolIdentifier() => IOFunc.printBool(retOp)
            case a: ArrayIdentifier => IOFunc.printPtr(retOp)
            case p: PairIdentifier => IOFunc.printPtr(retOp)
            case _ => throw new IllegalArgumentException("print: not an expression")
        }
    }

    def translate(node: PrintlnNode): Unit = {
        translate(new PrintNode(node.expr))
        IOFunc.println()
    }

    def translate(node: IfNode): Unit = {
        val ifBlock = IfBlock()
        val ifTrue = ifBlock.nextT
        val ifFalse = ifBlock.nextF
        val next = ifBlock.next
        val op = translate(node.expr)
        op match {
            case ImmVal(num) => {
                currInstBlock.addInst(
                    MovInst(r8, op),
                    CmpInst(r8, immTrue)
                )
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
        controlFlowFuncs += ((ifTrue.num.toString, ifTrue),
        (ifFalse.num.toString, ifFalse),
        (next.num.toString, next))
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
        controlFlowFuncs += ((cond.num.toString, cond),
        (loop.num.toString, loop),
        (next.num.toString, next))
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
            currInstBlock.addInst(
                CmpInst(op1, op2),
                MovCondInst(cond, r8, immTrue),
                MovCondInst(notcond, r8, immFalse),
                FreeRegister(op1)
            )
            r8
        }
        
        node match {
            case IntLiterNode(n) => {
                if (n >= 0) {ImmVal(n)}
                else {
                    currInstBlock.addInst(LdrPseudoInst(r8, n))
                    r8
                }
            }
            case BoolLiterNode(true) => {
                ImmVal(1)
            }
            case BoolLiterNode(false) => {
                ImmVal(0)
            }
            case CharLiterNode(c) => {
                val num = c.toInt
                ImmVal(num)
            }
            case StrLiterNode(s) => {
                val label = stringDef(s)
                val loadlabelAddrInstr = LabelAddress(label)
                val reg = TempRegister()
                currInstBlock.addInst(LdrInst(reg, loadlabelAddrInstr))
                reg
            }            
            case PairLiterNode() => {
                ImmVal(0)
            }
            case n: IdentNode => {
                Variable(n.newName)
            }
            case ArrayElemNode(ident, exprList) => {
                val op1 = translate(ident)
                currInstBlock.addInst(MovInst(r8, op1))
                for (expr <- exprList) {
                    currInstBlock.addInst(PushInst(r8))
                    val op2 = translate(expr)
                    currInstBlock.addInst(
                        MovInst(r10, op2),
                        PopInst(r8),
                        BranchLinkInst("_arrLoad")
                    )
                }
                r8
            }
            case NotNode(expr) => {
                var op = translate(expr)
                op match {
                    case ImmVal(num) => {
                        if (num == 0) op = immTrue
                        else op = immFalse
                        op 
                    }
                    // TODO: asInstanceOf or put all remaining cases
                    case r: Register => {
                        currInstBlock.addInst(
                            CmpInst(r, ImmVal(1)),
                            MovCondInst("NE", r8, ImmVal(1)),
                            MovCondInst("Eq", r8, ImmVal(0))
                        )
                        r8 
                    }
                }            
            }
            case NegNode(expr) => {
                val op = translate(expr)
                op match {
                    case ImmVal(num) => {
                        currInstBlock.addInst(
                            MovInst(r8, op), 
                            NegInst(r8, r8)
                        )
                    }
                    case r: Register => {
                        currInstBlock.addInst(NegInst(r8, r))
                    }
                }
                r8
            }
            case LenNode(expr) => {
                val op = translate(expr)
                val offset = ImmOffset(op.asInstanceOf[Register], ARRAY_LENGTH_OFFSET)
                currInstBlock.addInst(LdrInst(r8, offset))
                r8
            }
            case OrdNode(expr) => {
                val op = translate(expr)
                op match {
                    case ImmVal(num) => {
                        ImmVal(num)
                    }
                    case _: Register => {
                        op
                    }
                }
            }
            case ChrNode(expr) => {
                val op = translate(expr)
                op match {
                    case ImmVal(num) => {
                        ImmVal(num)
                    }
                    case _: Register => {
                        op
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
                    case ImmVal(num) => {
                    currInstBlock.addInst(MovInst(reg2, op2))
                    }
                    case r: Register => {
                        reg2 = r
                    }
                }
                currInstBlock.addInst(
                    SmullInst(r8, r9, reg1, reg2),
                    CmpInst(r9, ASR(r8, 31)),
                    BranchCondInst("NE", "_errOverflow"),
                    FreeRegister(reg1)
                )
                r8
            }
            case DivNode(fstexpr, sndexpr) => {
                currInstBlock.addInst(PushInst(r0, r1))
                val op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(r0, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(
                    MovInst(r1, op2),
                    CmpInst(r1, ImmVal(0)),
                    BranchCondInst("Eq", "_errDivZero"),
                    BranchLinkInst("__aeabi_idivmod"),
                    MovInst(r8, r0),
                    PopInst(r0, r1)
                )
                r8
            }
            case ModNode(fstexpr, sndexpr) => {
                currInstBlock.addInst(PushInst(r0, r1))
                val op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(r0, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(
                    MovInst(r1, op2),
                    CmpInst(r1, ImmVal(0)),
                    BranchCondInst("Eq", "_errDivZero"),
                    BranchLinkInst("__aeabi_idivmod"),
                    MovInst(r8, r1),
                    PopInst(r0, r1)
                )
                r8
            }
            case AddNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                op1 match {
                    case ImmVal(num) => {
                        val reg = TempRegister()
                        currInstBlock.addInst(MovInst(reg, op1))
                        val op2 = translate(sndexpr)
                        currInstBlock.addInst(
                            AddsInst(r8, reg, op2),
                            FreeRegister(reg)
                        )
                    }
                    case r: Register => {
                        val op2 = translate(sndexpr)
                        currInstBlock.addInst(AddsInst(r8, r, op2))
                    }
                }
                r8
            }
            case SubNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                op1 match {
                    case ImmVal(num) => {
                        val reg = TempRegister()
                        currInstBlock.addInst(MovInst(reg, op1))
                        val op2 = translate(sndexpr)
                        currInstBlock.addInst(
                            SubsInst(r8, reg, op2),
                            FreeRegister(reg)
                        )
                    }
                    case r: Register => {
                        val op2 = translate(sndexpr)
                        currInstBlock.addInst(SubsInst(r8, r, op2))
                    }
                }
                r8
            }
            case GTNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, "GT", "LE")
            }
            case GTENode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, "GE", "LT")
            }
            case LTNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, "LT", "GE")
            }
            case LTENode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, "LE", "GT")
            }
            case EqNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, "Eq", "NE")
            }
            case IEqNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, "NE", "Eq")
            }
            // TODO: fix label
            case AndNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister()
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                val reg2 = TempRegister()
                val newBlock = InstBlock()
                currInstBlock.addInst(
                    MovInst(reg2, op2),
                    CmpInst(reg1, immTrue), 
                    BranchNumCondInst("NE", newBlock.num),
                    CmpInst(reg2, immTrue)
                )
                currInstBlock = newBlock
                currInstBlock.addInst(
                    MovCondInst("Eq", r8, immTrue),
                    MovCondInst("NE", r8, immFalse),
                    FreeRegister(reg1),
                    FreeRegister(reg2)
                )
                controlFlowFuncs += ((newBlock.num.toString, newBlock))
                r8
            }
            case OrNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister()
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                val reg2 = TempRegister()
                val newBlock = InstBlock()
                currInstBlock.addInst(
                    MovInst(reg2, op2),
                    CmpInst(reg1, immTrue), 
                    BranchNumCondInst("Eq", newBlock.num),
                    CmpInst(reg2, immTrue)
                )
                currInstBlock = newBlock
                currInstBlock.addInst(
                    MovCondInst("Eq", r8, immTrue),
                    MovCondInst("NE", r8, immFalse),
                    FreeRegister(reg1),
                    FreeRegister(reg2)
                )
                controlFlowFuncs += ((newBlock.num.toString, newBlock))
                r8
            }
        }
    }
}
