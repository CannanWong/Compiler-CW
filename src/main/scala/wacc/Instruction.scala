package wacc

sealed trait Instruction

case class AddInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class AddsInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class SubInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class SubsInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class RsbInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class NegInst(rd: Register, rm: Register) extends Instruction
case class MulInst(rd: Register, rm: Register, op: Operand) extends Instruction
case class SmullInst(rdlo: Register, rdhi: Register, rm: Register, rs: Register) extends Instruction

case class CmpInst(rn: Register, op: Operand) extends Instruction
case class MovInst(rd: Register, op: Operand) extends Instruction
case class MovCondInst(condition: String, rd: Register, op: Operand) extends Instruction
case class AndInst(rd: Register, op: Operand) extends Instruction
case class OrInst(rd: Register, op: Operand) extends Instruction

case class LdrInst(rd: Register, op: Operand) extends Instruction
case class LdrPseudoInst(rd: Register, num: Int) extends Instruction
case class StrInst(rd: Register, op: Operand) extends Instruction
case class PushInst(regList: List[Register]) extends Instruction
case class PopInst(regList: List[Register]) extends Instruction
case class BranchInst(label: String) extends Instruction
case class BranchLinkInst(label: String) extends Instruction
case class BranchCondInst(condition: String, label: String) extends Instruction
case class BLEqInst(label: String) extends Instruction
case class BLNEInst(label: String) extends Instruction

case class Label() extends Instruction

// To be moved to assign register part
// sealed trait Register
// E.g. R1: Register(1)
// case class Register() {
//     var num = ControlFlowGraph.nextRegNum
//     ControlFlowGraph.nextRegNum += 1
// }


sealed trait Operand
sealed trait Register extends Operand
case class TempRegister() extends Register
case class FixedRegister(num: Int) extends Register
case class Variable(name: String) extends Register
//NEW: address of the label
case class LabelAddress(address: String) extends Operand
// TODO: replace string with identifier for type
case class ImmVal(num: Int, ty: TypeIdentifier) extends Operand
case class ASR(r: Register, bits: Int) extends Operand

