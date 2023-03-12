package wacc

import scala.collection.mutable.LinkedHashMap

object AssignRegisterOptimised {
  val BASIC_BLOCK_SIZE = 4
  val bbg = LinkedHashMap[String, BasicBlock]()

  // Formatting CFG into monomorphic Basic Block Flow-Graph
  def formatCFG(cfg: LinkedHashMap[String, FuncBlock]): Unit = {
    for ((name, block) <- cfg.toList) {
      var curCFGBlk: ControlFlowBlock = block.body
      // Entry dummy
      val entryBlk = BasicBlock(List.empty)
      var lstBasicBlk: BasicBlock = entryBlk
      do {
        curCFGBlk match {
          case i: InstBlock => {
            val instBasicBlk = BasicBlock(i.instList.toList)
            lstBasicBlk.succs.addOne(instBasicBlk)
            lstBasicBlk = instBasicBlk
            curCFGBlk = i.next
          }
          case i: IfBlock => {
            val trueBlk = BasicBlock(i.nextT.instList.toList)
            val falseBlk = BasicBlock(i.nextF.instList.toList)
            val nextDummy = BasicBlock(List.empty)
            lstBasicBlk.succs.addAll(List(trueBlk, falseBlk))
            trueBlk.succs.addOne(nextDummy)
            falseBlk.succs.addOne(nextDummy)
            //dummy block for connection
            lstBasicBlk = nextDummy
            curCFGBlk = i.next
          }
          case w: WhileBlock => {
            val condBlk = BasicBlock(w.cond.instList.toList)
            val loopBlk = BasicBlock(w.loop.instList.toList)
            val nextDummy = BasicBlock(List.empty)
            lstBasicBlk.succs.addOne(condBlk)
            condBlk.succs.addAll(List(loopBlk, nextDummy))
            loopBlk.succs.addOne(condBlk)
            lstBasicBlk = nextDummy
            curCFGBlk = w.next
          }
          case f: FuncBlock => {
            throw new IllegalStateException(
            "Funcblock shd not contain another funcblock, " +
            "this behaviour is not expected from code generator")
          }
        }
      } while (curCFGBlk != null)
      bbg.addOne((name, entryBlk))
    }
  }
  /*
  def liveVariableAnalysis(cfg: LinkedHashMap[String, FuncBlock]): 
      (Map[FuncBlock, Set[String]], Map[FuncBlock, Set[String]]) = {
    var liveIn = Map.empty[BasicBlock, Set[String]]
    var liveOut = Map.empty[BasicBlock, Set[String]]

    // initialize liveOut for all blocks to empty set
    cfg.foreach(block => liveOut += (block -> Set.empty))
    var reversedCFGNodes = cfg.toList.reverse

    // iterate until a fixed point is reached
    var changed = true
    while (changed) {
      changed = false

      // iterate over blocks in reverse order
      for (block <- reversedCFGNodes) {
        val newLiveOut = Set.empty[String]

        // calculate new liveOut set
        block.successors.foreach(successor => newLiveOut ++= liveIn(successor))

        // calculate new liveIn set
        val newLiveIn = block.use ++ (newLiveOut -- block.defs)

        // update liveIn and liveOut sets for this block
        if (newLiveIn != liveIn(block) || newLiveOut != liveOut(block)) {
          liveIn += (block -> newLiveIn)
          liveOut += (block -> newLiveOut)
          changed = true
        }
      }
    }

    (liveIn, liveOut)
}
*/
}
