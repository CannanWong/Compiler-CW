package wacc

sealed trait Instruction

case class AddInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class SubInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class MulInst(rd: Register, rm: Register, op: Operand) extends Instruction
case class CmpInst(rn: Register, op: Operand) extends Instruction
case class MovInst(rd: Register, op: Operand) extends Instruction
case class AndInst(rd: Register, op: Operand) extends Instruction
case class OrInst(rd: Register, op: Operand) extends Instruction
case class LdrInst(rd: Register, op: Operand) extends Instruction
case class StrInst(rd: Register, op: Operand) extends Instruction
case class PushInst(regList: List[Register]) extends Instruction
case class PopInst(regList: List[Register]) extends Instruction

// sealed trait Register
// E.g. R1: Register(1)
case class Register(num: Int)
sealed trait Operand
case class ImmVal(num: Int) extends  Operand
