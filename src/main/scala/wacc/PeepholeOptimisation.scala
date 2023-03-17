package wacc

import scala.collection.mutable._
import wacc.Constants._

object PeepholeOptimisation {
    /* Main function to be called, optimises each funcBlock */
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

    def optimiseBlock(instBlock: InstBlock): Unit = {
        val newInstList: ListBuffer[Instruction] = new ListBuffer()
        val iter = instBlock.instList.iterator
        while (iter.hasNext) {
            val inst = iter.next()
            newInstList ++= optimiseInst(inst, iter)  
        }
        instBlock.instList = newInstList
        instBlock.next match {
            case InstBlock() | IfBlock() | WhileBlock() => optimiseBlock(instBlock.next)
            case _ => 
        }
    }

    /* Returns the first instruction that isn't a redundant move instruction starting from inst */
    def removeRedundant(inst: Instruction, iter: Iterator[Instruction]): Option[Instruction] = {
        inst match {
            case MovInst(rd, op, _) => {
                if (rd == op) {
                    if (iter.hasNext) {
                        removeRedundant(iter.next(), iter)
                    } else None
                } else Some(inst)
            }
            case _ => Some(inst)
        }
    }

    /* Optimises the current instruction given one or more following instructions, 
       returning a list of optimised instructions */
    def optimiseInst(instruction: Instruction, iter: Iterator[Instruction]): ListBuffer[Instruction] = {
        var optimisedInsts: ListBuffer[Instruction] = new ListBuffer()
        val option = removeRedundant(instruction, iter)
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

        /* It has been checked that option is not empty and iter.hasNext is true */
        val inst = option.get
        inst match {
            /* Optimises out a load instruction immediately following a store instruction 
               for the same register and address */
            case StrInst(rd, op) => {
                optimisedInsts += inst
                removeRedundant(iter.next(), iter) match {
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
            /* Optimises the setup before a division or modulo operation, so that if the denominator is an ImmVal, 
               the compare instruction is removed, if the ImmVal is 0, it will branch directly to the error label, otherwise it will not*/
            case MovInst(FixedRegister(1), op, _) => {
                optimisedInsts += inst
                var next = iter.next()
                next match {
                    case CmpInst(FixedRegister(1), ImmVal(0)) => {
                        optimisedInsts += next
                        if (iter.hasNext) {
                            next = iter.next()
                            next match {
                                case BranchInst(ZERO_DIVISION_LABEL, _, Equal()) => {
                                    op match {
                                        case ImmVal(num) => {
                                            optimisedInsts = optimisedInsts.dropRight(1)
                                            num match {
                                                case 0 => {
                                                    optimisedInsts += BranchInst(ZERO_DIVISION_LABEL)
                                                }
                                                case _ => 
                                            }
                                        }
                                        case _ => optimisedInsts += next
                                    }
                                }
                                case _ => optimisedInsts ++= optimiseInst(next, iter)
                            }
                        }
                    }
                    case _ => optimisedInsts ++= optimiseInst(next, iter)
                }
            }
            /* Removes a move instruction that follows a reversed move instruction
               Eg. Mov r1, r2 followed by Mov r2, r1
               The second instruction is not needed. */
            case MovInst(rd, op, cond) => {
                optimisedInsts += inst
                if (iter.hasNext) {
                    val next = iter.next()
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
}
