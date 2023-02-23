package wacc

import scala.collection.mutable.ListBuffer
import java.nio.ReadOnlyBufferException

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
                var allocSize = 0
                exprList(0).typeVal() match {
                    case CharIdentifier() => allocSize = 1
                    case ArrayIdentifier(baseTy, dim) => ???
                    case PairIdentifier(ty1, ty2) => ???
                    case _ =>
                }
                val r0 = FixedRegister(0)
                val r8 = FixedRegister(8)
                val r12 = FixedRegister(12)
                var offset = 0

                // Calling malloc to get the address storing the array
                currInstBlock.addInst(MovInst(r0, 
                    ImmVal(4 + allocSize * exprList.length, IntIdentifier())))
                currInstBlock.addInst(BranchLinkInst("malloc"))
                currInstBlock.addInst(MovInst(r12, r0))

                // Storing the size of the array on the first 4 bytes / word
                currInstBlock.addInst(MovInst(r8, ImmVal(exprList.length, IntIdentifier())))
                currInstBlock.addInst(StrInst(r8, Offset(r12, offset)))

                // Iterate through the array to insert the elements
                for (expr <- exprList) {
                    offset += allocSize
                    currInstBlock.addInst(MovInst(r8, translate(expr)))
                    currInstBlock.addInst(StrInst(r8, Offset(r12, offset)))
                }

                // Storing the address into register or memory assigned for array
                currInstBlock.addInst(MovInst(Variable(node.ident.name), r8))
            }
            case NewPairNode(fstExpr, sndExpr) => {
                
            }
            case CallNode(ident, argList) => 
            case FstNode(lvalue) =>
            case SndNode(lvalue) => 
        }
    }
    def translate(node: LValuesAssignNode): Unit = {}
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
                        var inst3 = new MovEqInst(reg, new ImmVal(0, expr.typeVal()))
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
