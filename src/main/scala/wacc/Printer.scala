package wacc

import scala.collection.mutable.ListBuffer

object Printer {
    var output: ListBuffer[String] = ListBuffer.empty

    def printBlock(cfBlock: ControlFlowBlock): Unit = {
        cfBlock match {
            case ins: InstBlock => printBlock(ins: InstBlock)
            case con: IfBlock => printBlock(con: IfBlock)
            case whi: WhileBlock => printBlock(whi: WhileBlock)
            case call: CallBlock => printBlock(call: CallBlock)
            case fun: FuncBlock => printBlock(fun: FuncBlock)
            case _ => 
        }
    }

    def printBlock(instBlock: InstBlock): Unit = {
        // Print label
        for (inst <- instBlock.instList) {
            print(inst)
        }
        printBlock(instBlock.next)
    }

    def printBlock(ifBlock: IfBlock): Unit = {
        printBlock(ifBlock.cond)
        printBlock(ifBlock.nextT)
        printBlock(ifBlock.nextF)
        printBlock(ifBlock.next)
    }

    def printBlock(whileBlock: WhileBlock): Unit = {
        printBlock(whileBlock.cond)
        printBlock(whileBlock.loop)
        printBlock(whileBlock.next)
    }

    def printBlock(callBlock: CallBlock): Unit = {
        printBlock(callBlock.next)
    }

    def printBlock(funcBlock: FuncBlock): Unit = {
        output += funcBlock.directive.build()
        output += s"${funcBlock.name}:"
        printBlock(funcBlock.body)
    }

    /* ############### print instructions ############### */

    def print(inst: Instruction): Unit = {
        inst match {
            case inst: AddInst => 
                output += "add " + printOp(inst.rd, inst.rn, inst.op)
            case inst: SubInst => 
                output += "sub " + printOp(inst.rd, inst.rn, inst.op)
            case inst: MulInst => 
                output += "mul " + printOp(inst.rd, inst.rm, inst.op)
            case inst: CmpInst => 
                output += "cmp " + printOp(inst.rn, inst.op)
            case inst: MovInst => 
                output += "mov " + printOp(inst.rd, inst.op)
            // case inst: MovEqInst => 
            //     output += "moveq" + printOp(inst.rd, inst.op)
            // case inst: MovNEqInst => 
            //     output += "movneq" + printOp(inst.rd, inst.op)
            case inst: AndInst => 
                output += "and " + printOp(inst.rd, inst.op)
            case inst: OrInst => 
                output += "orr " + printOp(inst.rd, inst.op)
            case inst: LdrInst => 
                output += "ldr " + printOp(inst.rd, inst.op)
            case inst: StrInst => 
                output += "str " + printOp(inst.rd, inst.op)
            case inst: PushInst => 
                output += "push " + printOp(inst.regs)
            case inst: PopInst => 
                output += "pop " + printOp(inst.regs)
            case inst: BranchInst => 
                output += "B " + inst.label
            case inst: BranchLinkInst => 
                output += "BL " + inst.label
            case _ => output += "@ Unmatched instr"
        }
    }

    def printOp(op: Operand): String = {
        op match {
            case im: ImmVal => "#" + im.num.toString
            case r: FixedRegister => "r" + r.num.toString()
            case _ => "UAR!" // Unassigned register
        }
    }

    def printOp(reg: Register, op: Operand): String = {
        printOp(reg) + ", " + printOp(op)
    }

    def printOp(reg1: Register, reg2: Register, op: Operand): String = {
        printOp(reg1) + ", " + printOp(reg2) + ", " + printOp(op)
    }

    def printOp(regList: Seq[Register]): String = {
        var output = "{"
        for (reg <- regList) {
            output += printOp(reg) + ", "
        }
        output = output.dropRight(2) // remove ", "
        output += "}"
        output
    }
}