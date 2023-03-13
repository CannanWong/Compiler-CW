package wacc

import scala.collection.mutable._
import Constants._

object AssignRegisterOptimised {
  val BASIC_BLOCK_SIZE = 4

  // Basic Block Graphs are generated per funcBlock inside the complete CFG
  val basicBlockGraphs = LinkedHashMap[String, BasicBlockGraph]()

  //########################################################################//
  //                              CFG -> BBG                                //
  //########################################################################//

  // Formatting CFG into monomorphic Basic Block Flow-Graph
  def formatCFG(cfg: LinkedHashMap[String, FuncBlock]): Unit = {
    // Doing for every separate CFG function blocks, including main body as MAIN function
    for ((name, block) <- cfg.toList) {
      val bbg = BasicBlockGraph()
      var curCFGBlk: ControlFlowBlock = block.body
      // Entry dummy
      var lstBasicBlk: BasicBlock = bbg.entry
      do {
        curCFGBlk match {
          case i: InstBlock => {
            val instBasicBlk = BasicBlock(i.instList.toList)
            lstBasicBlk.succs.addOne(instBasicBlk)
            lstBasicBlk = instBasicBlk
            curCFGBlk = i.next
            instBasicBlk :: bbg.blocks
          }
          case i: IfBlock => {
            val trueBlk = BasicBlock(i.nextT.instList.toList)
            val falseBlk = BasicBlock(i.nextF.instList.toList)
            //dummy block for connection
            val nextDummy = BasicBlock(List.empty)
            lstBasicBlk.succs.addAll(List(trueBlk, falseBlk))
            trueBlk.succs.addOne(nextDummy)
            falseBlk.succs.addOne(nextDummy)
            lstBasicBlk = nextDummy
            curCFGBlk = i.next
            List(nextDummy, falseBlk, trueBlk) ::: bbg.blocks
          }
          case w: WhileBlock => {
            val condBlk = BasicBlock(w.cond.instList.toList)
            val loopBlk = BasicBlock(w.loop.instList.toList)
            //dummy block for connection
            val nextDummy = BasicBlock(List.empty)
            lstBasicBlk.succs.addOne(condBlk)
            condBlk.succs.addAll(List(loopBlk, nextDummy))
            loopBlk.succs.addOne(condBlk)
            lstBasicBlk = nextDummy
            curCFGBlk = w.next
            List(nextDummy, loopBlk, condBlk) ::: bbg.blocks
          }
          case f: FuncBlock => {
            throw new IllegalStateException(
            "Funcblock shd not contain another funcblock, " +
            "this behaviour is not expected from code generator")
          }
        }
      } while (curCFGBlk != null)
      basicBlockGraphs.addOne((name, bbg))
    }
  }

  def partitionBasicBlock(insts: List[Instruction]): List[BasicBlock] = {
    val partitions: List[BasicBlock] = List()
    var remainder = insts
    var nxtBlk: BasicBlock = null
    while (!remainder.isEmpty) {
      val block = BasicBlock(remainder.takeRight(BASIC_BLOCK_SIZE))
      remainder = remainder.dropRight(BASIC_BLOCK_SIZE)
      block.succs += nxtBlk
      block :: partitions
      nxtBlk = block
    }
    partitions
  }

  //########################################################################//
  //                          Live Variable Analysis                        //
  //########################################################################//

  def genUseDefs(block: BasicBlock): Unit = {
    for (i <- block.insts) {
      i match {
        // Ordinary 2/3 argument instructions
        case MovInst(rd, op, condition)         => genTwoOp(block, rd, op)
        case NegInst(rd, rm)                    => genTwoOp(block, rd, rm)
        case StrInst(rd, op)                    => genTwoOp(block, rd, op)
        case StrbInst(rd, op)                   => genTwoOp(block, rd, op)
        case StrChgInst(rd, op)                 => genTwoOp(block, rd, op)
        case StrbChgInst(rd, op)                => genTwoOp(block, rd, op)
        case LdrInst(rd, op)                    => genTwoOp(block, rd, op)
        case LdrsbInst(rd, op)                  => genTwoOp(block, rd, op)
        case LdrPseudoInst(rd, num)             => block.defs += rd
        case AddInst(rd, rn, op)                => genThreeOp(block, rd, rn, op)
        case AddsInst(rd, rn, op)               => genThreeOp(block, rd, rn, op)
        case SubInst(rd, rn, op)                => genThreeOp(block, rd, rn, op)
        case SubsInst(rd, rn, op)               => genThreeOp(block, rd, rn, op)
        case RsbInst(rd, rn, op)                => genThreeOp(block, rd, rn, op)
        case RsbsInst(rd, rn, op)               => genThreeOp(block, rd, rn, op)
        case MulInst(rd, rm, op)                => genThreeOp(block, rd, rm, op)
        case AndInst(rd, op)                    => genFlag(block, rd, op)
        case OrInst(rd, op)                     => genFlag(block, rd, op)
        case CmpInst(rn, op)                    => genFlag(block, rn, op)
        //! Not sure if this is how push and pop shd be handled
        case p: PushInst                        => for (r <- p.regs) block.uses += r
        case p: PopInst                         => for (r <- p.regs) block.defs += r
        case SmullInst(rdlo, rdhi, rm, rs)      => {
          genThreeOp(block, rdlo, rdhi, rm)
          block.uses ++= checkUses(rs)
        }
        case BranchInst(label, link, condition) => {
          if (link) {
            block.defs += r0
            block.uses ++= Set(r0, r1, r2, r3)
            //TODO: would be better to check for actual register usage from func table
          }
          //other branchs shd be condition or loop and can be ignored
        }
        //! Do we need anything on std functions (possibly arrStore and arrLoad)?
        case BranchNumInst(num, condition) => {
        }
        case FreeRegister(r) => //block.defs - r //? is this needed or shd freereg be removed from codeGenOptd?
        case WaccComment(s) => // Nothing
      }
    }
  }

  def genTwoOp(block: BasicBlock, rd: Register, rn: Operand): Unit = {
    block.defs += rd
    block.uses ++= checkUses(rn)
  }

  def genFlag(block: BasicBlock, rd: Register, rn: Operand): Unit = {
    block.uses ++= checkUses(rd) ++ checkUses(rn)
  }

  def genThreeOp(block: BasicBlock, rd: Register, rn: Register, op: Operand): Unit = {
    block.defs += rd
    block.uses ++= checkUses(rn) ++ checkUses(op)
  }

  //? Shd check if variable is corresponding to array/pair type?
  def checkUses(op: Operand): Set[Register] = {
    op match {
      case r: Register => Set(r)
      case _ => Set()
    }
  }
  def liveVariableAnalysis(): (Map[BasicBlock, Set[Register]], Map[BasicBlock, Set[Register]]) = {
    val liveIn = Map.empty[BasicBlock, Set[Register]]
    val liveOut = Map.empty[BasicBlock, Set[Register]]
    for ((name, bbg) <- basicBlockGraphs) {
      // initialize liveOut for all blocks to empty set
      bbg.blocks.foreach{ 
        block => {
          genUseDefs(block)
          liveOut += (block -> Set.empty)
        }
      }

      // iterate until a fixed point is reached
      var changed = true
      while (changed) {
        changed = false

        // iterate over blocks, which shd be in reverse order when inserted
        for (block <- bbg.blocks) {
          val newLiveOut: Set[Register] = Set.empty[Register]

          // calculate new liveOut set
          block.succs.foreach(successor => newLiveOut ++= liveIn(successor))

          // calculate new liveIn set
          val newLiveIn: Set[Register] = block.uses.addAll(newLiveOut.diff(block.defs))

          // update liveIn and liveOut sets for this block
          if (newLiveIn != liveIn(block) || newLiveOut != liveOut(block)) {
            liveIn += (block -> newLiveIn)
            liveOut += (block -> newLiveOut)
            changed = true
          }
        }
      }
    }
    (liveIn, liveOut)
  }

  //########################################################################//
  //                          Inteference Graph                             //
  //########################################################################//

  // Interference graph generation
  def generateInterferenceGraph(
    liveIn: Map[BasicBlock, Set[Register]],
    liveOut: Map[BasicBlock, Set[Register]]):
    Map[Register, Set[Register]] = {
    val ig = Map.empty[Register, Set[Register]]

    // add nodes to the interference graph for each variable that is live at any point
    for ((block, variables) <- (liveIn.keySet ++ liveOut.keySet).map(block => (block, liveIn.getOrElse(block, Set.empty) ++ liveOut.getOrElse(block, Set.empty)))) {
      for (v <- variables) {
        ig.getOrElseUpdate(v, Set.empty[Register])
      }
    }

    // add edges to the interference graph for each pair of variables that are live simultaneously
    //? not sure whether to use liveIn or liveOut here
    for ((block, variables) <- liveOut) {
      for (v <- variables) {
        for (w <- variables if v != w) {
          ig(v) += w
          ig(w) += v
        }
      }
    }
    ig
  }

  //########################################################################//
  //                         DSatur Graph Colouring                         //
  //########################################################################//

  def graphColouring(graph: Map[Register, Set[Register]]): Map[Register, Register] = {
    var spilledCount = 0
    // Identify precolored nodes
    val precolored = graph.keys.filter(fixedRegs.contains).toSet
    
    // Sort the uncolored nodes in decreasing order of their degree
    var nodes = graph.keys.filterNot(precolored.contains).toList.sortBy(-graph(_).size)
    
    // Initialise the color map with the precolored nodes
    val colorMap = Map[Register, Register]() ++ precolored.map(reg => reg -> reg)

    // DSatur algorithm to color the partially-precoloured graph
    while (!nodes.isEmpty) {
      // Sort by 
      // 1. Saturation degree
      // 2. Total degree
      // Placing the largest element at the front
      val maxDSaturNode = 
        nodes.sortBy(
        r => (-(graph(r).filterNot(colorMap.contains)).size, 
              -(graph(r).size))).head
      nodes = nodes.drop(1)
      val neighborColors = graph(maxDSaturNode).filterNot(colorMap.contains).map(r => colorMap(r))
      val availableColors = fixedRegs.diff(neighborColors.toList)

      // Simple spilling that assigns stack space permanently
      if (availableColors.isEmpty) {
        SpilledStackSpace(spilledCount) :: availableColors
        spilledCount += 1
      }

      colorMap.update(maxDSaturNode, availableColors.head)
    }

    colorMap
  }
}
