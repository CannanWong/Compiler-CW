package wacc

import wacc.Registers._

object FreqCodeBlocks {
  def allocSpc(spc: Int): List[Instruction] = { List(
    MovInst(r0, ImmVal(spc, IntIdentifier())),
    BranchLinkInst("malloc"),
    MovInst(r12, r0))
  }


/*
  def addReadLabelToData(text: String, printType: String, funckBlock: FuncBlock) : String = {
      val content = new StringBuilder()
      content ++= s"  .word ${text.length()}\n" +
                  s".L.${printType}_str0:\n" +
                  s"  .asciz \"${text}\"\n" +
                  ".text\n"
      funckBlock.labels.addToPrintDataSubsection(content.toString())
      // returns label string for text added to .data directive
      s".L.${printType}_str${funckBlock.labels.labelCount}"
    }
  
  def readIn(): FuncBlock = { 
    val 
    val readInInsts = List(
    PushInst(List(lr)),
    StrInst(r0, Offset(sp, -4)),
    SubInst(sp, sp, ImmVal(4, IntIdentifier())),
    MovInst(r1, sp)
    LdrInst(r0, )
  )
  }
  */
}
