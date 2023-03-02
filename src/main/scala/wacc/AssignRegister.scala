package wacc

import scala.collection.mutable._

object AssignRegister {
    // val ir2cfg = LinkedHashMap[String, FuncBlock]()     // Control flow graph: Hash map of FuncBlocks
    // var currInstBlock: InstBlock = FuncBlock().body     // ! None
    var newInstList: ListBuffer[Instruction] = ListBuffer.empty
    var regQueue: Queue[Int] = Queue(7, 6, 5, 4, 3, 2, 1, 0)    // Storing available registers: R7-R0

    // When no registers are available  
    var storeInst: Option[StrInst] = None   // To add instuction to store on stack
    var currFPOffset: Int = 0               // fp offset

    val varOpTable: Map[String, Operand] = Map()    // Table mapping variable name to operand
    val tempRegTable: Map[Int, Operand] = Map()     // Table mapping temporary register number to operand

    def assignCFG(cfg: LinkedHashMap[String, FuncBlock]): Unit = {
        for ((name, funcBlock) <- cfg) {
            assignBlock(funcBlock)
        }
    }

    def assignBlock(funcBlock: FuncBlock): Unit = assignBlock(funcBlock.body)

    def assignBlock(cfBlock: ControlFlowBlock): Unit = {
        cfBlock match {
            case ins: InstBlock => assignBlock(ins: InstBlock)
            case con: IfBlock => assignBlock(con: IfBlock)
            case whi: WhileBlock => assignBlock(whi: WhileBlock)
            case fun: FuncBlock => assignBlock(fun: FuncBlock)
            case _ => 
        }
    }

    def assignBlock(instBlock: InstBlock): Unit = {
        newInstList = ListBuffer.empty
        for (inst <- instBlock.instList) {
            newInstList += assignInst(inst)
            
            // Add instuction to store on stack when no registers are available
            storeInst match {
                case Some(inst) => {
                    newInstList += inst
                    storeInst = None
                }
                case _ => 
            }
        }
        instBlock.instList = newInstList
        
        instBlock.next match {
            case InstBlock() | IfBlock() | WhileBlock() => assignBlock(instBlock.next)
            case _ => 
        }
    }

    def assignBlock(ifBlock: IfBlock): Unit = {
        assignBlock(ifBlock.nextT)
        assignBlock(ifBlock.nextF)
        assignBlock(ifBlock.next)
    }

    def assignBlock(whileBlock: WhileBlock): Unit = {
        assignBlock(whileBlock.cond)
        assignBlock(whileBlock.loop)
        assignBlock(whileBlock.next)
    }

    // Change operand and assign register if needed
    def assignInst(i: Instruction): Instruction = {
        i match {
            case inst: AddInst => AddInst(assignReg(inst.rd), assignReg(inst.rn), assignOp(inst.op))
            case inst: AddsInst => AddsInst(assignReg(inst.rd), assignReg(inst.rn), assignOp(inst.op))
            case inst: SubInst => SubInst(assignReg(inst.rd), assignReg(inst.rn), assignOp(inst.op))
            case inst: SubsInst => SubsInst(assignReg(inst.rd), assignReg(inst.rn), assignOp(inst.op))
            case inst: RsbInst => RsbInst(assignReg(inst.rd), assignReg(inst.rn), assignOp(inst.op))
            case inst: NegInst => NegInst(assignReg(inst.rd), assignReg(inst.rm))
            case inst: MulInst => MulInst(assignReg(inst.rd), assignReg(inst.rm), assignOp(inst.op))
            case inst: SmullInst => SmullInst(assignReg(inst.rdlo), assignReg(inst.rdhi), assignReg(inst.rm), assignReg(inst.rs))
            
            case inst: CmpInst => CmpInst(assignReg(inst.rn), assignOp(inst.op))
            case inst: MovInst => MovInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: MovCondInst => MovCondInst(inst.condition, assignReg(inst.rd), assignOp(inst.op))
            case inst: AndInst => AndInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: OrInst => OrInst(assignReg(inst.rd), assignOp(inst.op))
            
            case inst: LdrInst => LdrInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: LdrPseudoInst => LdrPseudoInst(assignReg(inst.rd), inst.num)
            case inst: StrInst => StrInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: StrbInst => StrbInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: StrChgInst => StrChgInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: StrbChgInst => StrbChgInst(assignReg(inst.rd), assignOp(inst.op))
            
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
            // Free fixed register that is used by temporary register
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
            case a: ASR => ASR(assignReg(a.r), a.bits)
            case i: ImmOffset => ImmOffset(assignReg(i.r), i.offset)
            case r: RegOffset => RegOffset(assignReg(r.rm), assignReg(r.rn))
            case s: ScaledOffsetLSL => ScaledOffsetLSL(assignReg(s.rn), assignReg(s.rm), s.shift)
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
                    // From stack
                    case Some(im: ImmOffset) => {
                        newInstList += LdrInst(Constants.r8, im)
                        Constants.r8
                    }
                    case _ => {
                        // Get fixed register
                        if (!regQueue.isEmpty) {
                            val fReg = FixedRegister(regQueue.dequeue())
                            varOpTable.addOne(v.name, fReg)
                            fReg
                        }
                        // No available registers
                        else {
                            newInstList += SubInst(Constants.sp, Constants.sp, ImmVal(-4))
                            currFPOffset -= 4
                            val op = ImmOffset(Constants.fp, currFPOffset)
                            varOpTable.addOne(v.name, op)
                            storeInst = Some(StrInst(Constants.r8, op))
                            Constants.r8
                        }
                    }
                }
            }
            case t: TempRegister => assignReg(t)
            case _ => reg
        }
    }
    def assignReg(tReg: TempRegister): Register = {
        val reg = tempRegTable.get(tReg.num)
        reg match {
            case Some(fReg: FixedRegister) => fReg
            // From stack
            case Some(im: ImmOffset) => {
                newInstList += LdrInst(Constants.r8, im)
                Constants.r8
            }
            case _ => {
                // Get fixed register
                if (!regQueue.isEmpty) {
                    val fReg = FixedRegister(regQueue.dequeue())
                    tempRegTable.addOne(tReg.num, fReg)
                    fReg
                }
                // No more available registers
                else {
                    newInstList += SubInst(Constants.sp, Constants.sp, ImmVal(-4))
                    currFPOffset -= 4
                    val op = ImmOffset(Constants.fp, currFPOffset)
                    tempRegTable.addOne(tReg.num, op)
                    storeInst = Some(StrInst(Constants.r8, op))
                    Constants.r8
                }
            }
        }
    }
}