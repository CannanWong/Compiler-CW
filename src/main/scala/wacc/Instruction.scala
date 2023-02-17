package wacc

sealed trait Instruction

case class Add(rd: Register, rn: Register, op: Operand) extends Instruction
case class Sub(rd: Register, rn: Register, op: Operand) extends Instruction
case class Mul(rd: Register, rm: Register, op: Operand) extends Instruction
case class Cmp(rn: Register, op: Operand) extends Instruction
case class Mov(rd: Register, op: Operand) extends Instruction
case class And(rd: Register, op: Operand) extends Instruction
case class Or(rd: Register, op: Operand) extends Instruction
case class Ldr(rd: Register, op: Operand) extends Instruction
case class Str(rd: Register, op: Operand) extends Instruction
case class Push(regList: List[Register]) extends Instruction
case class Pop(regList: List[Register]) extends Instruction

// sealed trait Register
// E.g. R1: Register(1)
case class Register(num: Int)
sealed trait Operand
case class ImmVal(num: Int)
