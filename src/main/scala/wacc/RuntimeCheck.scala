package wacc

import wacc.Constants._

object RuntimeCheck {
  /* Checks */
  val ZERO_DIVISION_LABEL = "_errDivZero"
  val NULL_POINTER_LABEL = "_errNull"
  val BOUNDS_CHECK_LABEL = "_boundsCheck"

  val zeroDivMsg = "fatal error: division or modulo by zero\n"
  val nullPointerMsg = "fatal error: null pair dereferenced or freed\n"
  val boundsCheckMsg = "fatal error: array index %d out of bounds\n"

  def runtimeErrorMsg(label: String): FuncBlock = {
    val msg = label match {
      case ZERO_DIVISION_LABEL => zeroDivMsg
      case NULL_POINTER_LABEL => nullPointerMsg
      case BOUNDS_CHECK_LABEL => boundsCheckMsg
      case _ => throw new IllegalArgumentException(s"error type ${label} does not exist")
    }
    /* call func */
    val func = new FuncBlock()
    val text = func.labels.addTextLabelToData(msg, label)
    IOFunc.printString(new LabelAddress(text))
    func.body.addInst(
      List(
        new MovInst(r0, new ImmVal(255, new IntIdentifier())),
        new BranchLinkInst("exit")
      )
    )
    CodeGenerator.controlFlowFuncs.addOne(label, func)
    func
  }
}
