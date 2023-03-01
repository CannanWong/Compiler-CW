package wacc

import wacc.FixedRegister

object Registers {
  val r0 = FixedRegister(0)
  val r1 = FixedRegister(1)
  val r2 = FixedRegister(2)
  val r3 = FixedRegister(3)
  val r4 = FixedRegister(4)
  val r5 = FixedRegister(5)
  val r6 = FixedRegister(6)
  val r7 = FixedRegister(7)
  val r8 = FixedRegister(8)
  val r9 = FixedRegister(9)
  val r10 = FixedRegister(10)
  val r12 = FixedRegister(12)
  val sp = FixedRegister(13)
  val lr = FixedRegister(14)
  val pc = FixedRegister(15)
}