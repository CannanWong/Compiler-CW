package wacc

import wacc.Constants._
import java.util.ResourceBundle.Control
import wacc.CodeGenerator._

object RuntimeCheck {
  /* messages */
  val zeroDivMsg = "fatal error: division or modulo by zero"
  val nullPointerMsg = "fatal error: null pair dereferenced or freed"
  val overflowMsg = "fatal error: integer overflow or underflow occurred"
  val boundsCheckMsg = "fatal error: array index %d out of bounds"

  def runtimeErrorMsg(label: String): FuncBlock = {
    val msg = label match {
      case ZERO_DIVISION_LABEL => zeroDivMsg
      case NULL_POINTER_LABEL => nullPointerMsg
      case OVERFLOW_LABEL => overflowMsg
      case BOUNDS_CHECK_LABEL => boundsCheckMsg
      case _ => throw new IllegalArgumentException(s"error type ${label} does not exist")
    }
    /* call func */
    val func = new FuncBlock()
    // val calleeFunc = controlFlowGraph
    // CodeGenerator.switchCurrInstrBlock(func, func.currBlock)
    val prevBlock = currInstBlock
    currInstBlock = func.body
    val text = func.directive.addTextLabelToData(msg, label)
    currInstBlock.addInst(PushInst(r0, r1, r2, r3))
    currInstBlock.addInst(LdrInst(r1, LabelAddress(text)))
    currInstBlock.addInst(WaccComment("should add print inst block"))
    val printFunc = IOFunc.printString(r1)
    
    if (printFunc == null) {
      currInstBlock.addInst(WaccComment("did not add print inst block"))
    }else {currInstBlock.addInst(WaccComment("added print inst block"))}
    currInstBlock.addInst(PopInst(r0, r1, r2, r3))
    func.name = label
    func.body.addInst(
      MovInst(r0, ImmVal(255)),
      BranchLinkInst("exit")
    )
    CodeGenerator.controlFlowFuncs.addOne(IOFunc.PRINT_STR_LABEL, printFunc)
    CodeGenerator.controlFlowFuncs.addOne(label, func)
    currInstBlock = prevBlock
    func
  }
}
