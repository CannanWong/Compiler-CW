package wacc

import scala.collection.mutable.{ListBuffer, LinkedHashMap}
import wacc.Constants._

object CodeGenerator {
    /* .data directive stores all string declarations */
    var mainFunc = FuncBlock()
    var controlFlowGraph = FuncBlock()
    
    var currInstBlock = controlFlowGraph.body
    
    /* NEW: temporory design to accomodate label jumps */
    val controlFlowFuncs = LinkedHashMap[String, FuncBlock]()

        /* utility functions */
    def switchCurrInstrBlock(newFuncBlock: FuncBlock): Unit = {
        CodeGenerator.controlFlowGraph = newFuncBlock
        CodeGenerator.currInstBlock = newFuncBlock.body
    }

    def stringDef(string: String): String = {
        mainFunc.directive.addTextLabelToData(string)
    }

    /* translate functions */

    def translateAST(p: ProgramNode): Unit = {
        translateMain(p.stat)
        for (func <- p.funcList) {
            translate(func)
        }
    }

    def translateMain(stat: StatNode): Unit = {
        val mainFuncBlock = FuncBlock()
        mainFuncBlock.setGlobalMain()
        mainFuncBlock.name = "main"

        mainFunc = mainFuncBlock
        /* change current instruction block to func block */
        switchCurrInstrBlock(mainFunc)

        currInstBlock.addInst(new PushInst(fp, lr))
        /* TODO: push caller saved registers that will be used */
        currInstBlock.addInst(new PushInst(r4, r5, r6, r7, r8, r10, r12))
        /* ################################ */
        currInstBlock.addInst(new MovInst(fp, sp))
        /* TODO: assign args to callee saved register and stack pos */
        translate(stat)

        currInstBlock.addInst(new MovInst(r0, ImmVal(0)))
        currInstBlock.addInst(new MovInst(sp, fp))
        /* TODO: pop caller saved registers that are pushed */
        currInstBlock.addInst(new PopInst(r4, r5, r6, r7, r8, r10, r12))
        /* ################################ */
        currInstBlock.addInst(new PopInst(fp, pc))

        controlFlowFuncs.addOne(mainFuncBlock.name, mainFuncBlock)
    }

    def translate(f: FuncNode): Unit = {
        val funcBlock = FuncBlock()
        /* change current instruction block to func block */
        switchCurrInstrBlock(funcBlock)
        f match {
            case FuncNode(ty, ident, param, stat) => {
                /**
                  * TODO:
                    1. set up frame pointer and lr
                    2. get arg from stack / callee saved reg
                  */

                currInstBlock.addInst(new PushInst(fp, lr))
                /* TODO: push caller saved registers that will be used */
                currInstBlock.addInst(new PushInst(r4, r5, r6, r7, r8, r10, r12))
                /* ################################ */
                currInstBlock.addInst(new MovInst(fp, sp))
                /* TODO: assign args to callee saved register and stack pos */
                translate(stat)
                currInstBlock.addInst(new MovInst(sp, fp))
                /* TODO: pop caller saved registers that are pushed */
                currInstBlock.addInst(new PopInst(r4, r5, r6, r7, r8, r10, r12))
                /* ################################ */
                currInstBlock.addInst(new PopInst(fp, pc))

                funcBlock.name = if (funcBlock.GLOBAL_MAIN) "main" else s"wacc_${ident.name}"
                controlFlowFuncs.addOne(funcBlock.name, funcBlock)
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
            case n: SkipNode => translate(n: SkipNode)
            case n:AssignIdentNode => translate(n: AssignIdentNode)
            case n: LValuesAssignNode => translate(n: LValuesAssignNode)
            case n: ReadNode => translate(n: ReadNode)
            case n: FreeNode => translate(n: FreeNode)
            case n: ReturnNode => translate(n: ReturnNode)
            case n: ExitNode => translate(n: ExitNode)
            case n: PrintNode => translate(n: PrintNode)
            case n: PrintlnNode => translate(n: PrintlnNode)
            case n: IfNode => translate(n: IfNode)
            case n: WhileNode => translate(n: WhileNode)
            case n: BeginEndNode => translate(n: BeginEndNode)
            case n: StatJoinNode => translate(n: StatJoinNode)
        }
    }
    def translate(node: SkipNode): Unit = {}

    def translate(node: AssignIdentNode): Unit = {
    }

    def translate(node: LValuesAssignNode): Unit = {
    }

    def translate(node: ReadNode): Unit = {
        val retOp = new TempRegister()
        val exprTy = node.lvalue.typeVal()
        exprTy match {
            case CharIdentifier() => IOFunc.readChar(retOp)
            case IntIdentifier() => IOFunc.readInt(retOp)
            case _ =>
            // case _ => throw new IllegalArgumentException("print: not an int or char")
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
        currInstBlock.addInst(PushInst(r0, r1, r2, r3))

        val retOp = translate(node.expr)
        val exprTy = node.expr.typeVal()
        exprTy match {
            case CharIdentifier() => IOFunc.printChar(retOp)
            case IntIdentifier() => IOFunc.printInt(retOp)
            case StrIdentifier() => IOFunc.printString(retOp)
            case BoolIdentifier() => IOFunc.printBool(retOp)
            case a: ArrayIdentifier => IOFunc.printPtr(retOp)
            case p: PairIdentifier => IOFunc.printPtr(retOp)
            // anyIdentifier or null
            case  _ => IOFunc.printPtr(retOp)
        }
        currInstBlock.addInst(PopInst(r0, r1, r2, r3))
    }

    def translate(node: PrintlnNode): Unit = {
        translate(new PrintNode(node.expr))

        currInstBlock.addInst(PushInst(r0, r1, r2, r3))
        IOFunc.println()
        currInstBlock.addInst(PopInst(r0, r1, r2, r3))
    }

    def translate(node: IfNode): Unit = {
        val ifBlock = IfBlock()
        /* next block of current control flow graph block points to this ifBlock */
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
        currInstBlock.addInst(BranchNumCondInst(NOT_EQUAL, ifFalse.num))
        currInstBlock.next = ifBlock
        currInstBlock = ifTrue
        translate(node.fstStat)
        currInstBlock.addInst(BranchNumInst(next.num))
        currInstBlock = ifFalse
        translate(node.sndStat)
        currInstBlock = next
        // controlFlowFuncs += ((ifTrue.num.toString, ifTrue),
        // (ifFalse.num.toString, ifFalse),
        // (next.num.toString, next))
    }

    def translate(node: WhileNode): Unit = {
        val whileBlock = WhileBlock()
        val cond = whileBlock.cond
        val loop = whileBlock.loop
        val next = whileBlock.next
        currInstBlock.next = whileBlock
        currInstBlock = cond
        translate(node.expr)
        currInstBlock.addInst(BranchNumCondInst(NOT_EQUAL, next.num))
        currInstBlock = loop
        translate(node.stat)
        currInstBlock.addInst(BranchNumInst(cond.num))
        currInstBlock = next
        // controlFlowFuncs += ((cond.num.toString, cond),
        // (loop.num.toString, loop),
        // (next.num.toString, next))
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
                            MovCondInst(NOT_EQUAL, r8, ImmVal(1)),
                            MovCondInst(EQUAL, r8, ImmVal(0))
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
                    CmpInst(r9, ASR(r8, ImmVal(INT_HIGHEST_BIT))),
                    BranchCondInst(NOT_EQUAL, "_errOverflow"),
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
                    BranchCondInst(EQUAL, "_errDivZero"),
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
                    BranchCondInst(EQUAL, "_errDivZero"),
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
                    BranchNumCondInst(NOT_EQUAL, newBlock.num),
                    CmpInst(reg2, immTrue)
                )
                currInstBlock = newBlock
                currInstBlock.addInst(
                    MovCondInst(EQUAL, r8, immTrue),
                    MovCondInst(NOT_EQUAL, r8, immFalse),
                    FreeRegister(reg1),
                    FreeRegister(reg2)
                )
                // controlFlowFuncs += ((newBlock.num.toString, newBlock))
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
                    BranchNumCondInst(EQUAL, newBlock.num),
                    CmpInst(reg2, immTrue)
                )
                currInstBlock = newBlock
                currInstBlock.addInst(
                    MovCondInst(EQUAL, r8, immTrue),
                    MovCondInst(NOT_EQUAL, r8, immFalse),
                    FreeRegister(reg1),
                    FreeRegister(reg2)
                )
                // controlFlowFuncs += ((newBlock.num.toString, newBlock))
                r8
            }
        }
    }
}
