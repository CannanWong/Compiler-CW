package wacc

import scala.collection.mutable.ListBuffer

object ControlFlowGraph

sealed trait ControlFlowBlock

case class InstBlock(label: String, instList: ListBuffer[Instruction], next: InstBlock) extends ControlFlowBlock {
    def addInst(inst: Instruction) {
        instList += inst
    }
}

case class IfBlock(label: String, fst: InstBlock, snd: InstBlock) extends ControlFlowBlock

case class WhileBlock(label: String, expr: InstBlock, loop: InstBlock, next: InstBlock) extends ControlFlowBlock

case class CallBlock(label: String, func: FuncBlock, next: InstBlock) extends ControlFlowBlock

case class FuncBlock(label: String, body: List[Instruction]) extends ControlFlowBlock