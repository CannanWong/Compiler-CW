package wacc

import scala.collection.mutable._
import Constants._
import javax.management.InstanceNotFoundException
import ControlFlowGraph.{nextBBNum, nextTRNum}

object AssignRegisterOptimised {
  val BASIC_BLOCK_SIZE = 1
  val ARG_OFFSET = 4

  val varUsed = Set[Register]()

  //########################################################################//
  //                              CFG -> BBG                                //
  //########################################################################//

  // Formatting CFG into monomorphic Basic Block Flow-Graph
  def formatCFG(cfg: LinkedHashMap[String, FuncBlock]): LinkedHashMap[String, BasicBlockGraph] = {
    val basicBlockGraphs = LinkedHashMap[String, BasicBlockGraph]()
    // Doing for every separate CFG function blocks, including main body as MAIN function
    for ((name, block) <- cfg.toList) {
      val bbg = BasicBlockGraph()
      var curCFGBlk: ControlFlowBlock = block.body
      // Entry dummy
      var lstBasicBlk: BasicBlock = bbg.entry
      do {
        curCFGBlk match {
          case i: InstBlock => {
            val instBasicBlks = partitionBasicBlock(i.instList.toList)
            lstBasicBlk.succs.addOne(instBasicBlks.head)
            lstBasicBlk = instBasicBlks.last
            curCFGBlk = i.next
            bbg.blocks.addAll(instBasicBlks)
          }
          case i: IfBlock => {
            val trueBlks = partitionBasicBlock(i.nextT.instList.toList)
            val falseBlks = partitionBasicBlock(i.nextF.instList.toList)
            //dummy block for connection
            val nextDummy = BasicBlock(nextBBNum(), List.empty)
            lstBasicBlk.succs.addAll(List(trueBlks.head, falseBlks.head))
            trueBlks.last.succs.addOne(nextDummy)
            falseBlks.last.succs.addOne(nextDummy)
            lstBasicBlk = nextDummy
            curCFGBlk = i.next
            bbg.blocks.addAll(trueBlks)
            bbg.blocks.addAll(falseBlks)
            bbg.blocks.addOne(nextDummy)
          }
          case w: WhileBlock => {
            val condBlks = partitionBasicBlock(w.cond.instList.toList)
            val loopBlks = partitionBasicBlock(w.loop.instList.toList)
            //dummy block for connection
            val nextDummy = BasicBlock(nextBBNum(), List.empty)
            lstBasicBlk.succs.addOne(condBlks.head)
            condBlks.last.succs.addAll(List(loopBlks.head, nextDummy))
            loopBlks.last.succs.addOne(condBlks.head)
            lstBasicBlk = nextDummy
            curCFGBlk = w.next
            bbg.blocks.addAll(condBlks)
            bbg.blocks.addAll(loopBlks)
            bbg.blocks.addOne(nextDummy)
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
    basicBlockGraphs
  }

  def partitionBasicBlock(insts: List[Instruction]): List[BasicBlock] = {
    val partitions: ListBuffer[BasicBlock] = ListBuffer()
    var remainder = insts
    var lstBlk: BasicBlock = null
    if (remainder.isEmpty) {
      partitions.addOne(BasicBlock(nextBBNum(), List.empty))
    }
    // Partition is also done in reverse
    while (!remainder.isEmpty) {
      val block = BasicBlock(nextBBNum(), remainder.take(BASIC_BLOCK_SIZE))
      remainder = remainder.drop(BASIC_BLOCK_SIZE)
      if (lstBlk != null) {
        lstBlk.succs += block
      }
      partitions.addOne(block)
      lstBlk = block
    }
    partitions.toList
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
        case LdrPseudoInst(rd, num)             => {
          block.defs += rd
          varUsed += rd
        }
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
        //? Not sure if this is how push and pop shd be handled
        case p: PushInst                        => for (r <- p.regs) {
          block.uses += r
          varUsed += r
        }
        case p: PopInst                         => for (r <- p.regs) {
          block.defs += r
          varUsed += r
        }
        case SmullInst(rdlo, rdhi, rm, rs)      => {
          genThreeOp(block, rdlo, rdhi, rm)
          block.uses ++= checkUses(rs)
          varUsed += rs
        }
        case BranchInst(label, link, condition) => {
          // If link is false, it means the program is going to end anyway
          if (link) {
            block.defs ++= Set(r0, r1)
            block.uses ++= Set(r0, r1, r2, r3)
            varUsed ++= Set(r0, r1, r2, r3)
          }
        }
        case BranchNumInst(num, condition) => 
          // This is branching to main function body labels so shd contain no uses and defs
        case FreeRegister(r) => 
        case WaccComment(s) => // Nothing
      }
    }
  }

  def genTwoOp(block: BasicBlock, rd: Register, rn: Operand): Unit = {
    block.defs += rd
    varUsed += rd
    block.uses ++= checkUses(rn)
  }

  def genFlag(block: BasicBlock, rd: Register, rn: Operand): Unit = {
    block.uses ++= checkUses(rd) ++ checkUses(rn)
  }

  def genThreeOp(block: BasicBlock, rd: Register, rn: Register, op: Operand): Unit = {
    block.defs += rd
    varUsed += rd
    block.uses ++= checkUses(rn) ++ checkUses(op)
  }

  def checkUses(op: Operand): Set[Register] = {
    op match {
      case r: Register => {
        varUsed += r
        Set(r)
      }
      case ASR(r, _) => {
        varUsed += r
        Set(r)
      }
      case ImmOffset(r, offset) => {
        varUsed += r
        Set(r)
      }
      case RegOffset(rm, rn) => {
        varUsed += rm
        varUsed += rn
        Set(rm, rn)
      }
      case ScaledOffsetLSL(rn, rm, shift) => {
        varUsed += rm
        varUsed += rn
        Set(rm, rn)
      }
      case _ => Set()
    }
  }

  def liveVariableAnalysis(
    basicBlockGraphs: LinkedHashMap[String, BasicBlockGraph]): 
      Map[String, (Map[BasicBlock, Set[Register]], Map[BasicBlock, Set[Register]])] = {
    val liveRangeMap = 
      Map[String, 
      (Map[BasicBlock, Set[Register]], Map[BasicBlock, Set[Register]])]()
    for ((name, bbg) <- basicBlockGraphs) {
      val liveIn = Map.empty[BasicBlock, Set[Register]]
      val liveOut = Map.empty[BasicBlock, Set[Register]]
      // initialize liveOut for all blocks to empty set
      bbg.blocks.foreach{ 
        block => {
          genUseDefs(block)
          liveIn += (block -> Set.empty)
          liveOut += (block -> Set.empty)
          }
      }

      // iterate until a fixed point is reached
      var changed = true
      while (changed) {
        changed = false
        // iterate over blocks, which shd be in reverse order when inserted
        for (block <- bbg.blocks.reverse) {
          val newLiveOut: Set[Register] = Set.empty[Register]

          // calculate new liveOut set
          for (succ <- block.succs) {
            newLiveOut ++= liveIn(succ)
          }

          // calculate new liveIn set
          val newLiveIn: Set[Register] = block.uses ++ (liveOut(block).diff(block.defs))

          // update liveIn and liveOut sets for this block
          if (newLiveIn != liveIn(block) || newLiveOut != liveOut(block)) {
            liveIn.update(block, newLiveIn)
            liveOut.update(block, newLiveOut)
            changed = true
          }
        }
      }
      liveRangeMap.addOne(name, (liveIn, liveOut))
    }
    liveRangeMap
  }

  //########################################################################//
  //                          Inteference Graph                             //
  //########################################################################//

  // Interference graph generation
  def genIG(liveIn: Map[BasicBlock, Set[Register]], 
    liveOut: Map[BasicBlock, Set[Register]]): Map[Register, Set[Register]] = {
    // Add nodes to the interference graph for each variable that is live at any point
    val ig = Map.empty[Register, Set[Register]]
    val lives = (liveIn.values.toSet ++ liveOut.values.toSet)
    for (variables <- lives) {
      for (v <- variables) {
        ig.getOrElseUpdate(v, Set.empty[Register])
      }
    }

    // Add edges to the interference graph for each pair of variables that are live simultaneously
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

  def colouring(graph: Map[Register, Set[Register]], funcArgs: Map[Register, Register]): Map[Register, Register] = {
    var spilledCount = 0
    // Identify precolored nodes
    val precolored = graph.keys.filter(
      r => allFixedRegs.contains(r) || funcArgs.contains(r)).toSet
    
    // Sort the uncolored nodes in decreasing order of their degree
    var nodes = graph.keys.filterNot(precolored.contains).toList.sortBy(-graph(_).size)
    
    // Initialise the color map with the precolored nodes
    val colourMap = Map[Register, Register]() ++= precolored.map(reg => reg -> reg) ++= funcArgs

    // DSatur algorithm to color the partially-precoloured graph
    while (!nodes.isEmpty) {
      // Sort by 
      // 1. Saturation degree
      // 2. Total degree
      // Placing the largest element at the front
      val maxDSaturNode = 
        nodes.sortBy(
        r => (-(graph(r).filterNot(colourMap.contains)).size, 
              -(graph(r).size))).head
      nodes = nodes.drop(1)

      // Filter coloured nodes from neighbour nodes and extract the colours
      val neighborColours = 
        graph(maxDSaturNode)
        .filter(r => colourMap.contains(r))
        .map(colourMap.apply)

      // Filter assignable fixed registers with neighbour colours 
      // or registers taken by function arguments
      val availableColours: ListBuffer[Register] =
        ListBuffer().addAll(usablefixedRegs
          .filterNot(r => funcArgs.values.exists(_ == r))
          .filterNot(neighborColours.contains))

      // Simple spilling that assigns stack space permanently
      if (availableColours.isEmpty) {
        availableColours.addOne(SpilledStackSpace(spilledCount))
        spilledCount += 1
      }
      colourMap.update(maxDSaturNode, availableColours.head)
    }
    colourMap
  }

  //########################################################################//
  //                   Main Register Colouring Function                     //
  //########################################################################//

  def regColouringAlloc(cfg: LinkedHashMap[String, FuncBlock]): 
    Map[String, Map[Register, Register]] = {
    val regMap = Map[String, Map[Register, Register]]()
    
    // Basic Block Graphs are generated per funcBlock inside the complete CFG
    val basicBlockGraphs = formatCFG(cfg)
    val liveRanges = liveVariableAnalysis(basicBlockGraphs)
    for ((name, (liveIn, liveOut)) <- liveRanges) {
      val inteferenceGraph = genIG(liveIn, liveOut)
      val funcArgs = Map[Register, Register]()
      if (name != "main") {
        cfg.get(name).fold(throw new InstanceNotFoundException("funcblock not found"))(
          block => {
            // Load function arguments if this is a custom wacc function block
            var argCount = 0
            for (arg <- block.paramList) {
              if (argCount < 4) {
                funcArgs.addOne(Variable(arg.ident.newName), FixedRegister(argCount))
              }
              else {
                funcArgs.addOne(Variable(arg.ident.newName), ArgStackSpace(argCount - ARG_OFFSET))
              }
              argCount += 1
            }
          })
      }
      regMap.addOne(name, colouring(inteferenceGraph, funcArgs))
    }
    regMap
  }
  

  //########################################################################//
  //                        Helper / Test Functions                         //
  //########################################################################//

  def printLiveInOut(liveIn: Map[BasicBlock, Set[Register]], 
    liveOut: Map[BasicBlock, Set[Register]]): Unit = {
      println("liveIn:")
      liveIn.foreach(r => {
          print(s"${r._1.id}:  ")
          println(r._2)
      })
      println("#################################################")
      println("liveOut:")
      liveOut.foreach(r => {
          print(s"${r._1.id}:  ")
          println(r._2)
      })
      println("#################################################")
  }

  def colouringTest(): Unit = {
    val testGraph1 = Map[Register, Set[Register]]((Variable("a"), Set(Variable("a"), Variable("b"), Variable("c"), Variable("d"))),(Variable("b"), Set(Variable("b"), Variable("a"), Variable("c"), Variable("d"), Variable("e"))),(Variable("c"), Set(Variable("b"), Variable("c"), Variable("a"), Variable("d"), Variable("e"))),(Variable("d"), Set(Variable("a"), Variable("d"), Variable("c"), Variable("b"), Variable("e"))),(Variable("e"), Set(Variable("d"), Variable("e"), Variable("c"), Variable("b"))))
    val allVarSet = Set[Register]( Variable("a1"),  Variable("a2"),  Variable("a3"),  Variable("a4"),  Variable("a5"),  Variable("a6"),  Variable("a7"),  Variable("a8"),  Variable("a9"),  Variable("b1"),  Variable("b2"),  Variable("b3"),  Variable("b4"),  Variable("b5"),  Variable("b6"),  Variable("b7"),  Variable("b8"),  Variable("b9"),  Variable("c1"),  Variable("c2"),  Variable("c3") )
    val testGraph2 = Map[Register, Set[Register]]((Variable("a1"), allVarSet), (Variable("a2"), allVarSet), (Variable("a3"), allVarSet), (Variable("a4"), allVarSet), (Variable("a5"), allVarSet), (Variable("a6"), allVarSet), (Variable("a7"), allVarSet), (Variable("a8"), allVarSet), (Variable("a9"), allVarSet), (Variable("b1"), allVarSet), (Variable("b2"), allVarSet), (Variable("b3"), allVarSet), (Variable("b4"), allVarSet), (Variable("b5"), allVarSet), (Variable("b6"), allVarSet), (Variable("b7"), allVarSet), (Variable("b8"), allVarSet), (Variable("b9"), allVarSet), (Variable("c1"), allVarSet), (Variable("c2"), allVarSet), (Variable("c3"), allVarSet))
    val testGraph3 = Map[Register, Set[Register]](
        (Variable("a"), Set(FixedRegister(4), Variable("a"), Variable("b"), Variable("c"), Variable("d"), FixedRegister(8))),
        (Variable("b"), Set(FixedRegister(8), FixedRegister(4), Variable("b"), Variable("a"), Variable("c"), Variable("d"), Variable("e"), FixedRegister(6))),
        (Variable("c"), Set(FixedRegister(4), Variable("b"), Variable("c"), Variable("a"), Variable("d"), Variable("e"), FixedRegister(6))),
        (Variable("d"), Set(FixedRegister(8), FixedRegister(4), Variable("a"), Variable("d"), Variable("c"), Variable("b"), Variable("e"))),
        (Variable("e"), Set(FixedRegister(4), Variable("d"), Variable("e"), Variable("c"), Variable("b"), FixedRegister(6))),
        /**/(Variable("args1"), Set(Variable("a"), Variable("b"), Variable("c"))),
        (FixedRegister(8), Set(Variable("a"), Variable("b"), Variable("d"))),
        (FixedRegister(6), Set(Variable("c"), Variable("b"), Variable("e"))),
        (FixedRegister(4), Set(Variable("a"), Variable("b"), Variable("c"), Variable("d"), Variable("e")))
    )
    var colorMapping = colouring(testGraph1, Map[Register, Register]())
    println(colorMapping)
    colorMapping = colouring(testGraph2, Map[Register, Register]())
    println(colorMapping)
    colorMapping = colouring(testGraph3, Map[Register, Register](
        (Variable("args1"), FixedRegister(7))))
    println(colorMapping)
  }
    
  def lvaTest(): Unit = {
      val t0 = Variable("t0")
      val t1 = TempRegister(nextTRNum())
      val t2 = Variable("t2")
      //dummy to match slide example temp numbers for debugging
      TempRegister(nextTRNum())
      val t3 = TempRegister(nextTRNum())
      val t4 = TempRegister(nextTRNum())
      val cfg = LinkedHashMap[String, FuncBlock]()
      val main = FuncBlock()
      main.setGlobalMain()
      main.body = InstBlock()
      main.body.addInst(
          MovInst(t0, ImmVal(1)),
          MovInst(t1, ImmVal(10)),
          MovInst(t2, ImmVal(1)),
          BranchNumInst(2)
      )
      val whileBlock = WhileBlock()
      whileBlock.cond = InstBlock()
      whileBlock.cond.addInst(
          CmpInst(t1, t2),
          BranchNumInst(3, condition = GreaterThan()),
      )
      whileBlock.loop = InstBlock()
      whileBlock.loop.addInst(
          MovInst(t3, t2),
          MovInst(t4, t0),
          MulInst(t4, t3, t4),
          MovInst(t0, t4),
          AddInst(t2, t2, ImmVal(1))
      )
      whileBlock.next = InstBlock()
      whileBlock.next.addInst(
          MovInst(t2, t2)
      )
      main.body.next = whileBlock
      cfg.addOne("main", main)
      val bbgs = formatCFG(cfg)
      println("#################################################")
      println("Blocks: ")
      bbgs("main").blocks.foreach(b => if (!b.insts.isEmpty) println(b.insts))
      println("#################################################")
      val liveRangeMap = liveVariableAnalysis(bbgs)
      val (liveIn, liveOut) = liveRangeMap("main")
      val inteferenceGraph = genIG(liveIn, liveOut)
      val colorMap = colouring(inteferenceGraph, Map[Register, Register]())
      println(inteferenceGraph)
      println(colorMap)
  }
}
