package wacc

import scala.collection.mutable.ListBuffer

object ControlFlowGraph {
    var nextInstNum = 0
    var nextIfNum = 0
    var nextWhileNum = 0
    var nextCallNum = 0
    var nextFuncNum = 0
    var nextRegNum = 1
}

sealed trait ControlFlowBlock

case class InstBlock() extends ControlFlowBlock {
    var num: Int = ControlFlowGraph.nextInstNum
    var instList: ListBuffer[Instruction] = ListBuffer.empty
    var next: ControlFlowBlock = null
    ControlFlowGraph.nextInstNum += 1

    def addInst(inst: Instruction) = {
        instList += inst
    }
    def addInst(insts: List[Instruction]) = {
        for (inst <- insts) {
            instList += inst
        }
    }
}

case class IfBlock() extends ControlFlowBlock {
    var num: Int = ControlFlowGraph.nextIfNum
    var cond: InstBlock = new InstBlock()
    var nextT: InstBlock = new InstBlock()
    var nextF: InstBlock = new InstBlock()
    var next: InstBlock = new InstBlock()
    ControlFlowGraph.nextIfNum += 1
}

case class WhileBlock() extends ControlFlowBlock {
    var num: Int = ControlFlowGraph.nextWhileNum
    var cond: InstBlock = new InstBlock()
    var loop: InstBlock = new InstBlock()
    var next: InstBlock = new InstBlock()
    ControlFlowGraph.nextWhileNum += 1
}

case class CallBlock() extends ControlFlowBlock {
    var num: Int = ControlFlowGraph.nextCallNum
    var func: FuncBlock = null
    var next: InstBlock = new InstBlock()
    ControlFlowGraph.nextCallNum += 1
}

case class FuncBlock() extends ControlFlowBlock {
    var num: Int = ControlFlowGraph.nextFuncNum
    var param: InstBlock = new InstBlock()
    var body: InstBlock = new InstBlock()
    ControlFlowGraph.nextFuncNum += 1
}