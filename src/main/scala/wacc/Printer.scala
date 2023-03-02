package wacc

import scala.collection.mutable.ListBuffer

object Printer {
    var output: ListBuffer[String] = ListBuffer.empty

    def printBlock(cfBlock: ControlFlowBlock): Unit = {
        cfBlock match {
            case ins: InstBlock => printBlock(ins: InstBlock)
            case con: IfBlock => printBlock(con: IfBlock)
            case whi: WhileBlock => printBlock(whi: WhileBlock)
            //case call: CallBlock => printBlock(call: CallBlock)
            case fun: FuncBlock => printBlock(fun: FuncBlock)
            case _ => 
        }
    }

    def printBlock(instBlock: InstBlock): Unit = {
        // Print label
        for (inst <- instBlock.instList) {
            print(inst)
        }
        if (instBlock.next != null) {
            printBlock(instBlock.next)
        }
        
    }

    def printBlock(ifBlock: IfBlock): Unit = {
       // printBlock(ifBlock.cond)
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
            case inst: AddsInst =>
                output += "add " + printOp(inst.rd, inst.rn, inst.op)
            case inst: SubInst => 
                output += "sub " + printOp(inst.rd, inst.rn, inst.op)
            case inst: SubsInst =>
                output += "subs " + printOp(inst.rd, inst.rn, inst.op)
            case inst: RsbInst =>
                output += "rsb " + printOp(inst.rd, inst.rn, inst.op)
            case inst: NegInst =>
                output += "neg " + printOp(inst.rd, inst.rm)
            case inst: MulInst => 
                output += "mul " + printOp(inst.rd, inst.rm, inst.op)
            case inst: SmullInst =>
                output += "smull " + printOp(inst.rdlo, inst.rdhi, inst.rm, inst.rs)

            case inst: CmpInst =>
                output += "cmp " + printOp(inst.rn, inst.op)
            case inst: MovInst =>
                output += "mov " + printOp(inst.rd, inst.op)
            case inst: MovCondInst =>
                output += s"mov${inst.condition} " + printOp(inst.rd, inst.op)
            case inst: AndInst =>
                output += "and " + printOp(inst.rd, inst.op)
            case inst: OrInst =>
                output += "or " + printOp(inst.rd, inst.op)

            case inst: LdrInst =>
                output += "ldr " + printOp(inst.rd, inst.op)
            case inst: LdrPseudoInst =>
                output += "ldr " + printOp(inst.rd, LabelAddress(s"${inst.num}"))
            case inst: StrInst =>
                output += "strb " + printOp(inst.rd, inst.op) + "!"
            case inst: LdrSBInst =>
                output += "ldrsb " + printOp(inst.rd, inst.op)
            case inst: PushInst => 
                output += "push " + printRegList(inst.regs)
            case inst: PopInst => 
                output += "pop " + printRegList(inst.regs)

            case inst: BranchInst =>
                output += "b " + inst.label
            case inst: BranchCondInst =>
                output += s"b${inst.condition} " + inst.label
            case inst: BranchNumInst =>
                output += "b " + s".L${inst.num}"
            case inst: BranchNumCondInst =>
                output += s"b${inst.condition} " + s".L${inst.num}"
            case inst: BranchLinkInst =>
                output += "bl " + inst.label
            case inst: BranchLinkCondInst =>
                output += s"bl${inst.condtion} " + inst.label

            case inst: FreeRegister =>

            case _ => output += "@ Unmatched instr"
        }
    }

    /* ############### print operands ############### */

    def printOp(op: Operand): String = {
        op match {
            case im: ImmVal =>
                "#" + im.num.toString
            case r: FixedRegister =>
                printReg(r)
            case asr: ASR =>
                s"${printOp(asr.r)}, asr ${printOp(asr.bits)}"
            case off: ImmOffset =>
                s"[${printOp(off.r)}, #${off.offset}]"
            case ladr: LabelAddress => 
                s"=${ladr.address}"
            case soff: ScaledOffsetLSL => 
                s"[${printOp(soff.rn)}, ${printOp(soff.rm)}, lsl ${printOp(soff.shift)}]"
            
            case treg: TempRegister =>
                "UAR!" // Unassigned register
            case uvar: Variable =>
                "UAR!" // Unassigned register
            case _ =>
                throw new IllegalArgumentException(s"${op} is not legal arguement")
        }
    }

    def printOp(label: String) = {
        label
    }

    def printOp(ops: Operand*): String = {
        if (ops.isEmpty)
            ""
        else
            ops
            .map(op => printOp(op))
            .mkString(", ")
    }

    def printRegList(regList: Seq[Register]): String = {
        val regStr = if (regList.isEmpty)
                        ""
                    else
                        regList.map(reg => printOp(reg)).mkString(", ")
        s"{${regStr}}"        
    }

    def printReg(r: FixedRegister): String = {
        r.num match {
            case 11 => "fp"
            case 13 => "sp"
            case 14 => "lr"
            case 15 => "pc"
            case _ => s"r${r.num}"
        } 
    }
}