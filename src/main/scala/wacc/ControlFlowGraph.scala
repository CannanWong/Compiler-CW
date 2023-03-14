package wacc

import scala.collection.mutable.ListBuffer

// keeps track of next control flow blocks in use in the program generated
object ControlFlowGraph {
    var nextInstNum = 0
    var nextIfNum = 0
    var nextWhileNum = 0
    var nextCallNum = 0
    var nextFuncNum = 0
    var nextTempRegNum = 1

// resets the contol flow graph counter
    def resetCFG(): Unit = {
        nextInstNum = 0
        nextIfNum = 0
        nextWhileNum = 0
        nextCallNum = 0
        nextFuncNum = 0
        nextTempRegNum = 1
    }
}

sealed trait ControlFlowBlock

case class InstBlock() extends ControlFlowBlock {
    val num: Int = ControlFlowGraph.nextInstNum
    var instList: ListBuffer[Instruction] = ListBuffer.empty
    var next: ControlFlowBlock = null
    ControlFlowGraph.nextInstNum += 1

    def addInst(insts: Instruction*) = {
        for (inst <- insts) {
            instList += inst
        }
    }
    def addInst(insts: List[Instruction]) = {
        for (inst <- insts) {
            instList += inst
        }
    }
}

case class IfBlock() extends ControlFlowBlock {
    // index of the if block
    val num: Int = ControlFlowGraph.nextIfNum
    var nextT: InstBlock = InstBlock()
    var nextF: InstBlock = InstBlock()
    var next: InstBlock = InstBlock()
    ControlFlowGraph.nextIfNum += 1
}

case class WhileBlock() extends ControlFlowBlock {
    // index of the while block
    val num: Int = ControlFlowGraph.nextWhileNum
    var cond: InstBlock = InstBlock()
    var loop: InstBlock = InstBlock()
    var next: InstBlock = InstBlock()
    ControlFlowGraph.nextWhileNum += 1
}

case class FuncBlock() extends ControlFlowBlock {
    // boolean flag to indicate whether the current fucntion block is main
    var GLOBAL_MAIN = false
    // index of the function block
    val num: Int = ControlFlowGraph.nextFuncNum
    var body: InstBlock = InstBlock()
    var currBlock: InstBlock = body
    var name: String = ""
    var paramList: List[ParamNode] = List.empty
    // directive of the function to provide additional information to the assembler
    var directive: DataDirectiveStat = DataDirectiveStat()
    ControlFlowGraph.nextFuncNum += 1

    def setGlobalMain(): Unit = {
        GLOBAL_MAIN = true
        directive.GLOBAL_MAIN = true
    }
}