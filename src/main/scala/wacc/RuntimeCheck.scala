package wacc

import wacc.Constants._
import wacc.CodeGenerator._

/* returns function for reoprting runtime errors */
object RuntimeCheck {
    /* messages */
    val zeroDivMsg = "fatal error: division or modulo by zero"
    val nullPointerMsg = "fatal error: null pair dereferenced or freed"
    val overflowMsg = "fatal error: integer overflow or underflow occurred"
    val boundsCheckMsg = "fatal error: array index %d out of bounds"

    /* returns the runtime error message return function corresponding to the label parameter*/
    def runtimeErrorMsg(label: String): FuncBlock = {
        val msg = label match {
          case ZERO_DIVISION_LABEL => zeroDivMsg
          case NULL_POINTER_LABEL => nullPointerMsg
          case OVERFLOW_LABEL => overflowMsg
          case BOUNDS_CHECK_LABEL => boundsCheckMsg
          case _ => throw new IllegalArgumentException(s"error type ${label} does not exist")
        }
        /* Call function */
        val func = FuncBlock()
        val prevBlock = currInstBlock
        currInstBlock = func.body
        val text = func.directive.addTextLabelToData(msg, label)
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
