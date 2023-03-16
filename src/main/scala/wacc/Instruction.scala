package wacc

sealed trait Instruction

case class AddInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class AddsInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class SubInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class SubsInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class RsbInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class RsbsInst(rd: Register, rn: Register, op: Operand) extends Instruction
case class NegInst(rd: Register, rm: Register) extends Instruction
case class MulInst(rd: Register, rm: Register, op: Operand) extends Instruction
case class SmullInst(rdlo: Register, rdhi: Register, rm: Register, rs: Register) extends Instruction

case class CmpInst(rn: Register, op: Operand) extends Instruction
case class MovInst(rd: Register, op: Operand, condition: Condition = NoCondition()) extends Instruction
// case class MovCondInst(condition: String, rd: Register, op: Operand) extends Instruction
case class AndInst(rd: Register, op: Operand) extends Instruction
case class OrInst(rd: Register, op: Operand) extends Instruction

case class LdrInst(rd: Register, op: Operand) extends Instruction
case class LdrsbInst(rd: Register, op: Operand) extends Instruction
case class LdrPseudoInst(rd: Register, num: Int) extends Instruction
case class StrInst(rd: Register, op: Operand) extends Instruction
case class StrbInst(rd: Register, op: Operand) extends Instruction
case class StrChgInst(rd: Register, op: Operand) extends Instruction
case class StrbChgInst(rd: Register, op: Operand) extends Instruction
case class PushInst(regs: Register*) extends Instruction
case class PopInst(regs: Register*) extends Instruction

case class BranchInst(label: String, link: Boolean = false, condition: Condition = NoCondition()) extends Instruction
// case class BranchCondInst(condition: String, label: String) extends Instruction
case class BranchNumInst(num: Int, condition: Condition = NoCondition()) extends Instruction
// case class BranchNumCondInst(condition: String, num: Int) extends Instruction
// case class BranchLinkInst(label: String) extends Instruction
// case class BranchLinkCondInst(condtion: String, label: String) extends Instruction

// Useful for assigning registers, not actual arm instruction
case class FreeRegister(r: Register) extends Instruction
case class WaccComment(s: String) extends Instruction

sealed trait Operand
sealed trait Register extends Operand
case class TempRegister(num: Int) extends Register {
    // Automatically assign the next number
    override def toString() = s"TR$num"
}

case class FixedRegister(num: Int) extends Register {
    override def toString = s"FR$num"
}
case class Variable(name: String) extends Register {
    override def toString() = s"V$name"
}

case class SpilledStackSpace(id: Int) extends Register
case class ArgStackSpace(id: Int) extends Register

//NEW: address of the label
case class LabelAddress(address: String) extends Operand
case class ImmVal(num: Int) extends Operand {
    override def toString = s"$num"
}
case class ASR(r: Register, bits: ImmVal) extends Operand
case class ImmOffset(r: Register, offset: Int) extends Operand
case class RegOffset(rm: Register, rn: Register) extends Operand
case class ScaledOffsetLSL(rn: Register, rm: Register, shift: ImmVal) extends Operand