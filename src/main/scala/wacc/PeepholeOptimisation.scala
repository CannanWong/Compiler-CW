package wacc

import scala.collection.mutable._
import wacc.Constants._

object PeepholeOptimisation {
    def peepholeOptimise(cfg: LinkedHashMap[String, FuncBlock]): Unit = {
        for ((name, funcBlock) <- cfg) {
            optimiseBlock(funcBlock)
        }
    }

    def optimiseBlock(cfBlock: ControlFlowBlock): Unit = {
        cfBlock match {
            case ins: InstBlock => optimiseBlock(ins: InstBlock)
            case con: IfBlock => optimiseBlock(con: IfBlock)
            case whi: WhileBlock => optimiseBlock(whi: WhileBlock)
            case fun: FuncBlock => optimiseBlock(fun: FuncBlock)
            case _ => 
        }
    }

    def optimiseBlock(funcBlock: FuncBlock): Unit = {
        optimiseBlock(funcBlock.body)
        
    }

    def removeRedundant(inst: Instruction, iter: Iterator[Instruction]): Option[Instruction] = {
        inst match {
            case MovInst(rd, op, _) => {
                if (rd == op) {
                    if (iter.hasNext) {
                        removeRedundant(iter.next, iter)
                    } else None
                } else Some(inst)
            }
            case _ => Some(inst)
        }
    }

    def optimiseInst(instruction: Instruction, iter: Iterator[Instruction]): ListBuffer[Instruction] = {
        var optimisedInsts: ListBuffer[Instruction] = new ListBuffer()
        var option = removeRedundant(instruction, iter)
        option match {
            case Some(inst) => {
                if (!iter.hasNext) {
                    optimisedInsts += inst
                    return optimisedInsts
                }
            }
            case None => {
                return optimisedInsts
            }
        }

        // It has been checked that option is not empty and iter.hasNext is true
        val inst = option.get
        inst match {
            case StrInst(rd, op) => {
                optimisedInsts += inst
                removeRedundant(iter.next, iter) match {
                    case Some(next) => {
                        next match {
                            case LdrInst(rd1, op1) => {
                                if (!(rd == rd1 && op == op1)) {
                                    optimisedInsts ++= optimiseInst(next, iter)
                                } else {
                                    optimisedInsts += inst
                                }
                            }
                            case _ => {
                                optimisedInsts ++= optimiseInst(next, iter)
                            }
                        }
                    }
                    case None =>    
                }
            }
            case i: PushInst => {
                var pushInsts: ListBuffer[PushInst] = ListBuffer(i)
                var optionNext: Option[Instruction] = removeRedundant(iter.next, iter)

                while (optionNext.contains(PushInst)) {
                    pushInsts += optionNext.get.asInstanceOf[PushInst]
                    if (iter.hasNext) {
                        optionNext = removeRedundant(iter.next, iter)
                    } else {
                        optionNext = None
                    }
                }

                var matchingRegs: Boolean = true
                while (matchingRegs && optionNext.contains(PopInst)) {
                    if (optionNext.get.asInstanceOf[PopInst].regs == pushInsts.last.regs) {
                        pushInsts = pushInsts.dropRight(1)
                        if (iter.hasNext) {
                            optionNext = removeRedundant(iter.next, iter)
                        } else {
                            optionNext = None
                        }
                    } else {
                        matchingRegs = false
                    }
                }
                optimisedInsts ++= pushInsts
                optionNext match {
                    case Some(next) => {
                        optimisedInsts ++= optimiseInst(next, iter)
                    }
                    case None =>
                }
            }
            case MovInst(rd, op, cond) => {
                optimisedInsts += inst
                if (iter.hasNext) {
                    val next = iter.next
                    next match {
                        case MovInst(rd1, op1, cond1) => {
                            if (!(rd1 == op && op1 == rd && cond1 == cond)) {
                                optimisedInsts ++= optimiseInst(next, iter)
                            }
                        }
                        case _ => optimisedInsts ++= optimiseInst(next, iter)
                    }
                }
            }
            case _ => {
                optimisedInsts += inst
            }
        }
        optimisedInsts
    }
    
    def optimiseBlock(instBlock: InstBlock): Unit = {
        var newInstList: ListBuffer[Instruction] = new ListBuffer()
        val iter = instBlock.instList.iterator
        while (iter.hasNext) {
            var inst = iter.next
            newInstList ++= optimiseInst(inst, iter)  
        }
        instBlock.instList = newInstList
        instBlock.next match {
            case InstBlock() | IfBlock() | WhileBlock() => optimiseBlock(instBlock.next)
            case _ => 
        }
    }

    def optimiseBlock(ifBlock: IfBlock): Unit = {
        optimiseBlock(ifBlock.nextT)
        optimiseBlock(ifBlock.nextF)
        optimiseBlock(ifBlock.next)
    }

    def optimiseBlock(whileBlock: WhileBlock): Unit = {
        optimiseBlock(whileBlock.cond)
        optimiseBlock(whileBlock.loop)
        optimiseBlock(whileBlock.next)
    }
}
