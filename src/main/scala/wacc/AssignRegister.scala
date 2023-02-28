package wacc

import scala.collection.mutable

object AssignRegister {
    var nextRegNum = 7  // R7-R0
    val map: mutable.Map[Variable, Operand] = mutable.Map()


    def assign(instBlock: InstBlock): Unit = {
        for (inst <- instBlock.instList) {
            inst match {
                case AddInst(rd, rn, op) =>
                    rd match {
                        case t: TempRegister => {
                            val fReg = FixedRegister(nextRegNum)
                            nextRegNum += 1
                            // ! Replace instruction
                        }
                        case v: Variable => {
                            if (nextRegNum >= 0) {
                                val fReg = FixedRegister(nextRegNum)
                                map.addOne(v, fReg)
                                nextRegNum += 1
                                // ! Replace instruction
                            }
                            else {
                                // ! Stack
                                // ! Replace instruction
                            }
                        }
                        case _ =>
                    }
                
                case _ => // ! TODO
            }
        }
    }
}