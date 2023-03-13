package wacc

import scala.collection.mutable._
import wacc.Constants._

object PeepholeOptimisation {
    def peepholeOptimise(cfg: LinkedHashMap[String, FuncBlock]): Unit = {
        for ((name, funcBlock) <- cfg) {
            optimiseBlock(funcBlock)
        }
    }

    def optimiseBlock(funcBlock: FuncBlock): Unit = {
        optimiseBlock(funcBlock.body)
    }

    def notRedundant(inst: Instruction): Boolean = {
        inst match {
            case MovInst(rd, op, _) => {
                if (rd == op) {
                    false
                } else true
            }
            case _ => true
        }
    }

    def optimiseBlock(instBlock: InstBlock): Unit = {
        var newInstList: ListBuffer[Instruction] = ListBuffer.empty
        val iterator = instBlock.instList.iterator
        while (iterator.hasNext) {
            var inst = iterator.next
            inst match {
                case StrInst(rd, op) => {
                    newInstList += inst
                    if (iterator.hasNext) {
                        var next = iterator.next
                        next match {
                            case LdrInst(rd1, op1) => {
                                if (!(rd == rd1 && op == op1)) {
                                    newInstList += next
                                }
                            }
                            case _ => {
                                if (notRedundant(next)) {
                                    newInstList += next
                                }
                            }
                        }
                    }
                }
                case _ => {
                    if (notRedundant(inst)) {
                        newInstList += inst
                    }
                }
            }
        }
        instBlock.instList = newInstList
    }

}
