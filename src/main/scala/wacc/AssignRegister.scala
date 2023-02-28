package wacc

import scala.collection.mutable._


object AssignRegister {
    var IR2 = new FuncBlock()
    var currInstBlock = IR2.body
    var nextRegNum = 7  // R7-R0
    val varOpTable: Map[String, Operand] = Map()
    var rdTemp: Register = TempRegister()
    var rnTemp: Register = TempRegister()
    var opTemp: Operand = TempRegister()

    def assignBlock(instBlock: InstBlock): Unit = {
        for (inst <- instBlock.instList) {
            currInstBlock.addInst(assignInst(inst))
        }
        val nextBlock = instBlock.next
        nextBlock match {
            case InstBlock() | IfBlock() | WhileBlock() | CallBlock() | FuncBlock() => {
                currInstBlock.next = nextBlock
                // ! To be fixed
                // assignBlock(nextBlock)
            }
        }
    }

    def assignBlock(ifBlock: IfBlock): Unit = {
        currInstBlock = ifBlock.cond
        assignBlock(ifBlock.cond)
        currInstBlock = ifBlock.nextT
        assignBlock(ifBlock.nextT)
        currInstBlock = ifBlock.nextF
        assignBlock(ifBlock.nextF)
        currInstBlock = ifBlock.next
        assignBlock(ifBlock.next)
    }

    def assignBlock(whileBlock: WhileBlock): Unit = {
        currInstBlock = whileBlock.cond
        assignBlock(whileBlock.cond)
        currInstBlock = whileBlock.loop
        assignBlock(whileBlock.loop)
        currInstBlock = whileBlock.next
        assignBlock(whileBlock.next)
    }

    def assignBlock(callBlock: CallBlock): Unit = {
        currInstBlock = callBlock.next
        assignBlock(callBlock.next)
    }

    def assignBlock(funcBlock: FuncBlock): Unit = {
        currInstBlock = funcBlock.body
        assignBlock(funcBlock.body)
    }


    // Change operand and assign register if needed
    def assignInst(i: Instruction): Instruction = {
        i match {
            case inst: AddInst => AddInst(assignReg(inst.rd), assignReg(inst.rn), assignOp(inst.op))
            case inst: SubInst => SubInst(assignReg(inst.rd), assignReg(inst.rn), assignOp(inst.op))
            case inst: MulInst => MulInst(assignReg(inst.rd), assignReg(inst.rm), assignOp(inst.op))
            case inst: CmpInst => CmpInst(assignReg(inst.rn), assignOp(inst.op))
            case inst: MovInst => MovInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: AndInst => AndInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: OrInst => OrInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: LdrInst => LdrInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: StrInst => StrInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: PushInst => {
                val newRegList: ListBuffer[Register] = ListBuffer.empty
                for (reg <- inst.regs) {
                    newRegList += assignReg(reg)
                }
                PushInst(newRegList.toSeq:_*)
            }
            case inst: PopInst => {
                val newRegList: ListBuffer[Register] = ListBuffer.empty
                for (reg <- inst.regs) {
                    newRegList += assignReg(reg)
                }
                PopInst(newRegList.toSeq:_*)
            }
            case inst: BranchInst => inst
            case inst: BranchLinkInst => inst
        }
    }
    
    // Change operand and assign register if needed
    def assignOp(op: Operand): Operand = {
        op match {
            case t: TempRegister => assignReg(t)
            case v: Variable => assignReg(v)
            case _ => op
        }
    }

    def assignReg(reg: Register): Register = reg
    /*
    // Change register and assign register if needed
    def assignReg(reg: Register): Register = {
        case t: TempRegister => {
            val fReg = TempRegister()
            if (nextRegNum >= 0) {
                fReg = FixedRegister(nextRegNum)
                nextRegNum -= 1
            }
            else {
                // ! Stack
                // ! Replace instruction
            }
            fReg
        }
        case v: Variable => {
            val reg = varOpTable.get(v.name)
            reg match {
                case Some(fReg: FixedRegister) => fReg
                case _ => {
                    if (nextRegNum >= 0) {
                        val fReg = FixedRegister(nextRegNum)
                        varOpTable.addOne(v.name, fReg)
                        nextRegNum -= 1
                        fReg
                    }
                    else {
                        val fReg = TempRegister()
                        // ! Stack
                        // ! Replace instruction
                        fReg
                    }
                }
            }
        }
        case _ => reg
    }
    */
}