package wacc

import wacc.Registers._

object FreqCodeBlocks {
  def allocSpc(spc: Int): List[Instruction] = {
    List(MovInst(r0, ImmVal(spc, IntIdentifier())),
      BranchLinkInst("malloc"),
      MovInst(r12, r0))
  }
}
