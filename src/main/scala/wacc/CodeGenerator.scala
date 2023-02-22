package wacc

import scala.collection.mutable.ListBuffer

object CodeGenerator {
    var controlFlowGraph = new InstBlock()
    var currInstBlock = controlFlowGraph
    /* NEW: temporory design to accomodate print label jumps */
    var controlFlowFuncs = ListBuffer[FuncBlock]()

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
    def translate(node: AssignIdentNode): Unit = {}
    def translate(node: LValuesAssignNode): Unit = {}
    def translate(node: ReadNode): Unit = {}
    def translate(node: FreeNode): Unit = {}
    def translate(node: ReturnNode): Unit = {
        var op = translate(node.expr)
        var inst = new MovInst(new FixedRegister(0), op)
        currInstBlock.addInst(inst)
    }
    def translate(node: ExitNode): Unit = {
        // currInstBlock.addInst()
        var op = translate(node.expr)
        var inst = new MovInst(new FixedRegister(0), op)
        var inst2 = new BranchLinkInst("exit")
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
        val printInstr = new Print()
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
        node match {
            case IntLiterNode(n) => {
                return new ImmVal(n, node.typeVal())
            }
            case BoolLiterNode(true) => {
                return new ImmVal(1, node.typeVal())
            }
            case BoolLiterNode(false) => {
                return new ImmVal(0, node.typeVal())
            }
            case CharLiterNode(c) => {
                var num = c.toInt
                return new ImmVal(num, node.typeVal())
            }
            case StrLiterNode(s) => {
                ???
            }            
            case PairLiterNode() => {
                return new ImmVal(0, node.typeVal())
            }
            case NotNode(expr) => {
                var op = translate(expr)
                op match {
                    case ImmVal(num, t) => {
                        if (num == 0) op = new ImmVal(1, t)
                        else op = new ImmVal(0, t)
                        return op 
                    }
                    case _ => {
                        var inst = new CmpInst(op.asInstanceOf[Register], new ImmVal(1, expr.typeVal()))
                        var reg = new TempRegister()
                        var inst2 = new MovNEqInst(reg, new ImmVal(1, expr.typeVal()))
                        var inst3 = new MovEqInst(reg, new ImmVal(0, expr.typeVal))
                        currInstBlock.addInst(List(inst, inst2, inst3))
                        // var inst4 = new MovInst(op, reg)
                        return reg 
                    }
                }            
            }
            // case AddNode(fstexpr, sndexpr) => {
                // val rd = new Register(1)
                // val rn = new Register(2)
                // val op = new ImmVal(1)
                // val inst = new AddInst(rd, rn, op)
                // currBlock.addInst(inst)
            //}
        }
    }
}
