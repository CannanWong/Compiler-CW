package wacc

import scala.collection.mutable.ListBuffer

object CodeGenerator {
    var controlFlowGraph = new InstBlock("B1", ListBuffer.empty, null)
    var currBlock = controlFlowGraph

    def translate(f: FuncNode): Unit = {

    }
    def translate(s: StatNode): Unit = {

    }
    def translate(l: LValueNode): Unit = {

    }
    def translate(r: RValueNode): Unit = {
        r match {
            case AddNode(fstexpr, sndexpr) => {
                val rd = new Register(1)
                val rn = new Register(2)
                val op = new ImmVal(1)
                val inst = new AddInst(rd, rn, op)
                currBlock.addInst(inst)
            }
        }

    }
}
