package wacc

import wacc.Constants._
import java.util.ResourceBundle.Control
import wacc.CodeGenerator._

object RuntimeCheck {
  /* messages */
  val zeroDivMsg = "fatal error: division or modulo by zero\n"
  val nullPointerMsg = "fatal error: null pair dereferenced or freed\n"
  val overflowMsg = "fatal error: integer overflow or underflow occurred\n"
  val boundsCheckMsg = "fatal error: array index %d out of bounds\n"

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
    // val printFunc = IOFunc.printString(LabelAddress(text))
    func.name = label
    func.body.addInst(
      LdrInst(r0, LabelAddress(text)),
      BranchLinkInst(IOFunc.PRINT_STR_LABEL),
      MovInst(r0, ImmVal(255)),
      BranchLinkInst("exit")
    )
    CodeGenerator.controlFlowFuncs.addOne(label, func)
    currInstBlock = prevBlock
    func
  }
}
