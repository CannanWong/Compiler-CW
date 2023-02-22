package wacc

import scala.collection.mutable.ListBuffer

object Print {
    var output: ListBuffer[String] = ListBuffer.empty

    def printBlock(instBlock: InstBlock): Unit = {
        for (inst <- instBlock.instList) {
            print(inst)
        }
    }
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
            case inst: MovEqInst => 
                output += "" + printOp(inst.rd, inst.op)
            case inst: MovNEqInst => 
                output += "" + printOp(inst.rd, inst.op)
            case inst: AndInst => 
                output += "and " + printOp(inst.rd, inst.op)
            case inst: OrInst => 
                output += "orr " + printOp(inst.rd, inst.op)
            case inst: LdrInst => 
                output += "ldr " + printOp(inst.rd, inst.op)
            case inst: StrInst => 
                output += "str " + printOp(inst.rd, inst.op)
            case inst: PushInst => 
                output += "push " + printOp(inst.regList)
            case inst: PopInst => 
                output += "pop " + printOp(inst.regList)
            case inst: BranchInst => 
                output += "B " + inst.label
            case inst: BranchLinkInst => 
                output += "BL " + inst.label
        }
    }

    def printOp(op: Operand): String = {
        op match {
            case im: ImmVal => "#" + {
                im.t match {
                    case i: IntIdentifier => im.num.toString()
                    case _ => "" // ! To be completed
                }
            }
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

    def printOp(regList: List[Register]): String = {
        var output = "{"
        for (reg <- regList) {
            output += printOp(reg) + ", "
        }
        output = output.dropRight(2) // remove ", "
        output += "}"
        output
    }
}