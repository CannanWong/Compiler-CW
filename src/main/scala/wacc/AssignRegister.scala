package wacc

import scala.collection.mutable._
import wacc.Constants._

object AssignRegister {
    var newInstList: ListBuffer[Instruction] = ListBuffer.empty
    // Storing available registers: R7-R0
    var regQueue: Queue[FixedRegister] = Queue(r7, r6, r5, r4, r3, r2, r1, r0)

    // When no registers are available  
    var storeInst: Option[StrInst] = None           // To add instuction to store on stack
    var currFPOffset: Int = 0                       // fp offset

    val varOpTable: Map[String, Operand] = Map()    // Table mapping variable name to operand
    val tempRegTable: Map[Int, Operand] = Map()     // Table mapping temporary register number to operand

    var r9Used = false                              // Store whether r9 is used
    val interRegs = List(r8, r9, r10)
    

    val FUNCTION_FP_OFFSET = 36

    // ! ################### Optimisation ########################
    var optimiseFlag = false
    var regMap: Map[String, Map[Register, Register]] = Map()
    var curColorMap: Map[Register, Register] = Map()
    val stackAssigned: Map[Int, Operand] = Map()
    val argStack: Map[Int, Operand] = Map()
    // ! ################### Optimisation ########################

    def resetRegQueue(): Unit = {
        regQueue = Queue(r7, r6, r5, r4, r3, r2, r1, r0)
    }

    // Update the CFG in IR1 recursively and replace variables and temporary registers
    // with fixed registers
    def assignCFG(cfg: LinkedHashMap[String, FuncBlock]): Unit = {
        for ((name, funcBlock) <- cfg) {
            assignBlock(funcBlock)
        }
    }

    def assignBlock(funcBlock: FuncBlock): Unit = {
        resetRegQueue()
        // Only use r7-r4 if in main
        if (funcBlock.GLOBAL_MAIN) {
            regQueue = Queue(r7, r6, r5, r4)
        }
        val paramReg: Queue[FixedRegister] = Queue(r0, r1, r2, r3)
        val paramQueue: Queue[ParamNode] = Queue(funcBlock.paramList: _*)
        // Add the first 4 parameters mapping to r0-r3
        for (i <- 1 to 4) {
            if (!paramQueue.isEmpty) {
                varOpTable.addOne(paramQueue.dequeue().ident.newName, paramReg.dequeue())
                val revRegQueue = regQueue.reverse
                revRegQueue.dequeue()
                regQueue = revRegQueue.reverse
            }
        }
        // Starting from the last parameter
        val revParamQueue: Queue[ParamNode] = paramQueue.reverse
        currFPOffset = FUNCTION_FP_OFFSET
        var arg_count = revParamQueue.size
        while (!revParamQueue.isEmpty) {
            varOpTable.addOne(revParamQueue.dequeue().ident.newName, ImmOffset(fp, currFPOffset))
            if (optimiseFlag) {
                arg_count -= 1
                argStack.addOne(arg_count, ImmOffset(fp, currFPOffset))
            }
            currFPOffset += 4
        }
        currFPOffset = 0

        // ! ################### Optimisation ########################
        curColorMap = regMap.getOrElse(funcBlock.name, allFixedRegsMap)
        //println(curColorMap.foreach(b => {
        //    b._1 match {
        //        case _: Variable | _: TempRegister => println(b)
        //        case _ =>
        //    }
        //}))
        // ! ################### Optimisation ########################
        assignBlock(funcBlock.body)
    }

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
            case inst: RsbsInst => RsbsInst(assignReg(inst.rd), assignReg(inst.rn), assignOp(inst.op))
            case inst: NegInst => NegInst(assignReg(inst.rd), assignReg(inst.rm))
            case inst: MulInst => MulInst(assignReg(inst.rd), assignReg(inst.rm), assignOp(inst.op))
            case inst: SmullInst => SmullInst(assignReg(inst.rdlo), assignReg(inst.rdhi), assignReg(inst.rm), assignReg(inst.rs))
            
            case inst: CmpInst => CmpInst(assignReg(inst.rn), assignOp(inst.op))
            case inst: MovInst => MovInst(assignReg(inst.rd), assignOp(inst.op), inst.condition)
                /*{
                val reg1 = assignReg(inst.rd)
                val op = assignOp(inst.op)
                if (interRegs.contains(reg1) && interRegs.contains(op)) {
                    inst.rd match {
                        case Variable(name) => storeInst = Some(StrInst(reg1, varOpTable.get(name).get))
                        case _ => 
                    }
                }
                MovInst(reg1, op, inst.condition)
            }*/
            // case inst: MovCondInst => MovCondInst(inst.condition, assignReg(inst.rd), assignOp(inst.op))
            case inst: AndInst => AndInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: OrInst => OrInst(assignReg(inst.rd), assignOp(inst.op))  

            case inst: LdrInst => LdrInst(assignReg(inst.rd), assignOp(inst.op))
            case inst: LdrsbInst => LdrsbInst(assignReg(inst.rd), assignOp(inst.op))
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
                            case Some(f: FixedRegister) => regQueue.enqueue(f)
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

    def assignRegOp(reg: Register): Register = {
        reg match {
            case FixedRegister(num) => reg
            case _ => { 
                curColorMap.get(reg) match { 
                    case Some(value) => {
                        value match {
                            case SpilledStackSpace(id) => {
                                spilledReg(id)
                            }
                            case ArgStackSpace(id) => {
                                getStackArg(id)
                            }
                            case _ => value
                        }
                    }
                    case None => r12
                }
            }
        }
    }

    def getStackArg(id: Int): Register = {
        val op = argStack(id)
        newInstList += LdrInst(r9, op)
        storeInst = Some(StrInst(r9, op))
        r9
    }

    def spilledReg(id: Int): Register = {
        stackAssigned.get(id) match {
            case Some(value) => {
                newInstList += LdrInst(r9, value)
            }
            case None => {
                newInstList += SubInst(sp, sp, ImmVal(4))
                currFPOffset -= 4
                val op = ImmOffset(fp, currFPOffset)
                stackAssigned.addOne((id, op))
                storeInst = Some(StrInst(r9, op))
            }
        }
        r9
    }

    // Change register and assign register if needed
    def assignReg(reg: Register): Register = {
        // ! ################### Optimisation ########################
        if (optimiseFlag) {
            assignRegOp(reg)
        // ! ################### Optimisation ########################
        } else {
            reg match {
            case v: Variable => {
                val reg = varOpTable.get(v.name)
                reg match {
                    case Some(fReg: FixedRegister) => fReg
                    // From stack
                    case Some(im: ImmOffset) => {
                        if (!r9Used) {
                            r9Used = true
                            newInstList += LdrInst(r9, im)
                            r9
                        }
                        else {
                            r9Used = false
                            newInstList += LdrInst(r10, im)
                            r10
                        }
                    }
                    case _ => {
                        // Get fixed register
                        if (!regQueue.isEmpty) {
                            val fReg = regQueue.dequeue()
                            varOpTable.addOne(v.name, fReg)
                            fReg
                        }
                        // No available registers, store in stack using fp offset
                        else {
                            newInstList += SubInst(sp, sp, ImmVal(4))
                            currFPOffset -= 4
                            val op = ImmOffset(fp, currFPOffset)
                            varOpTable.addOne(v.name, op)
                            storeInst = Some(StrInst(r8, op))
                            r8
                        }
                    }
                }
            }
            case t: TempRegister => assignReg(t)
            case _ => reg
        }}
    }
    def assignReg(tReg: TempRegister): Register = {
        // ! ################### Optimisation ########################
        if (optimiseFlag) {
            assignRegOp(tReg)
        // ! ################### Optimisation ########################
        } else {
            val reg = tempRegTable.get(tReg.num)
            reg match {
                case Some(fReg: FixedRegister) => fReg
                // From stack
                case Some(im: ImmOffset) => {
                    if (!r9Used) {
                        r9Used = true
                        newInstList += LdrInst(r9, im)
                        r9
                    }
                    else {
                        r9Used = false
                        newInstList += LdrInst(r10, im)
                        r10
                    }
                }
                case _ => {
                    // Get fixed register
                    if (!regQueue.isEmpty) {
                        val fReg = regQueue.dequeue()
                        tempRegTable.addOne(tReg.num, fReg)
                        fReg
                    }
                    // No available registers, store in stack using fp offset
                    else {
                        newInstList += SubInst(sp, sp, ImmVal(4))
                        currFPOffset -= 4
                        val op = ImmOffset(fp, currFPOffset)
                        tempRegTable.addOne(tReg.num, op)
                        storeInst = Some(StrInst(r8, op))
                        r8
                    }
                }

            }
        }
    }
}