package wacc

sealed trait Instruction

case class AddInst(vd: Variable, vn: Variable, op: Operand) extends Instruction
case class SubInst(vd: Variable, vn: Variable, op: Operand) extends Instruction
case class MulInst(vd: Variable, vm: Variable, op: Operand) extends Instruction
case class CmpInst(vn: Variable, op: Operand) extends Instruction
case class MovInst(vd: Variable, op: Operand) extends Instruction
case class AndInst(vd: Variable, op: Operand) extends Instruction
case class OrInst(vd: Variable, op: Operand) extends Instruction
case class LdrInst(vd: Variable, op: Operand) extends Instruction
case class StrInst(vd: Variable, op: Operand) extends Instruction
case class PushInst(varList: List[Variable]) extends Instruction
case class PopInst(varList: List[Variable]) extends Instruction

// To be moved to assign register part
// sealed trait Register
// E.g. R1: Register(1)
// case class Register() {
//     var num = ControlFlowGraph.nextRegNum
//     ControlFlowGraph.nextRegNum += 1
// }

case class Variable() {
    var name: String = null
}
sealed trait Operand
case class ImmVal(num: Int) extends Operand
