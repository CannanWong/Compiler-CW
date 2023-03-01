package wacc

import scala.collection.mutable._


object AssignRegister {
    var IR2 = new FuncBlock()
    var currInstBlock = IR2.body
    var regQueue = Queue(7, 6, 5, 4, 3, 2, 1, 0)    // R7-R0
    val varOpTable: Map[String, Operand] = Map()
    val tempRegTable: Map[Int, Operand] = Map()

    def assignBlock(cfBlock: ControlFlowBlock): Unit = {
        cfBlock match {
            case InstBlock() | IfBlock() | WhileBlock() | CallBlock() | FuncBlock() =>
                assignBlock(cfBlock)
        }
    }

    def assignBlock(instBlock: InstBlock): Unit = {
        for (inst <- instBlock.instList) {
            currInstBlock.addInst(assignInst(inst))
        }
        instBlock.next match {
            case InstBlock() | IfBlock() | WhileBlock() | CallBlock() | FuncBlock() => 
                assignBlock(instBlock.next)
            case null => 
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
            case inst: FreeRegister => {
                inst.r match {
                    case t: TempRegister => {
                        tempRegTable.get(t.num) match {
                            case Some(f: FixedRegister) => regQueue.enqueue(f.num)
                            case _ =>
                        }
                        inst
                    }
                    case _ => inst
                }
            }
            case _ => i
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

    // Change register and assign register if needed
    def assignReg(reg: Register): Register = {
        reg match {
            case v: Variable => {
                val reg = varOpTable.get(v.name)
                reg match {
                    case Some(fReg: FixedRegister) => fReg
                    case _ => {
                        if (!regQueue.isEmpty) {
                            val fReg = FixedRegister(regQueue.dequeue())
                            varOpTable.addOne(v.name, fReg)
                            fReg
                        }
                        else {
                            // ! Stack
                            // ! Replace instruction
                            TempRegister()
                        }
                    }
                }
            }
            case t: TempRegister => assignReg(t)
            case _ => reg
        }
    }
    def assignReg(tReg: TempRegister): Operand = {
        if (!regQueue.isEmpty) {
            val fReg = FixedRegister(regQueue.dequeue())
            tempRegTable.addOne(tReg.num, fReg)
            fReg
        }
        // No more available registers
        else {
            // ! Stack
            // ! Replace instruction
            TempRegister()
        }
    }
}