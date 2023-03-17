package wacc

import wacc.FixedRegister
import wacc.ImmVal
import scala.collection.mutable._

/* registers */
object Constants {
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
  val fp = FixedRegister(11)
  val r12 = FixedRegister(12)
  val sp = FixedRegister(13)
  val lr = FixedRegister(14)
  val pc = FixedRegister(15)

  val allFixedRegs: List[FixedRegister] = List(r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, fp, r12, sp, lr, pc)
  val allFixedRegsMap: Map[Register, Register] = Map()
  allFixedRegs.foreach(reg => allFixedRegsMap.addOne(reg, reg))
  val usablefixedRegs: List[FixedRegister] = List(r7, r6, r5, r4, r3, r2, r1, r0, r10, r8, r9) //? r12?

  /* true and false immediate value */
  val immTrue = ImmVal(1)
  val immFalse = ImmVal(0)

  /* data offset */
  val ARRAY_LENGTH_OFFSET: Int = -4
  val INT_SIZE = -4
  val BYTE_SIZE = -1
  
  /* returns offset of data type of the opposite offset direction */
  def data_offset(dataType :Int): Int = {
    dataType * -1
  }

  /* highest bit of TYPR INT*/
  val INT_HIGHEST_BIT = 31

  /* enum for standard function */
  object StdFuncsEnum extends Enumeration {
    type StdFuncsEnum = Value

    val ZeroDivErr = Value(0, ZERO_DIVISION_LABEL)
    val NullErr = Value(1, NULL_POINTER_LABEL)
    val OverflowErr = Value(2, OVERFLOW_LABEL)
    val BoundsErr = Value(3, BOUNDS_CHECK_LABEL)
    val ArrStr = Value(4, ARRAY_STORE_LABEL)
    val ArrStrb = Value(5, ARRAY_STORE_B_LABEL)
    val ArrLdr = Value(6, ARRAY_LOAD_LABEL)
    val ArrLdrB = Value(7, ARRAY_LOAD_B_LABEL)
    val FreeP = Value(8, FREE_PAIR_LABEL)
  }

  /* text label for standard functions */
  val ZERO_DIVISION_LABEL = "_errDivZero"
  val NULL_POINTER_LABEL = "_errNull"
  val OVERFLOW_LABEL = "_errOverflow"
  val BOUNDS_CHECK_LABEL = "_boundsCheck"
  val ARRAY_STORE_LABEL = "_arrStore"
  val ARRAY_STORE_B_LABEL = "_arrStoreB"
  val ARRAY_LOAD_LABEL = "_arrLoad"
  val ARRAY_LOAD_B_LABEL = "_arrLoadB"
  val FREE_PAIR_LABEL = "_freePair"

  /* cond code (pg. 1-43 of mannual) */

  // val NOT_EQUAL = "ne"
  // val EQUAL = "eq"
  // val LESS_THAN = "lt"
  // val GREATER_THAN = "gt"
  // val GREATER_OR_EQUAL = "ge"
  // val LESS_OR_EQUAL = "le"
  // val OVERFLOW = "vs"
  // val NO_OVERFLOW = "vc"

/* charracter set of leagal escape characters in Wacc*/
  val ESCAPE_CHAR_LIST = 
    List[String]("\"", "\n", "\'", "\r", "\f", "\t", "\b", "\u0000", "\\")
}

sealed trait Condition
case class NoCondition() extends Condition {
  override def toString(): String = ""
} 
case class NotEqual() extends Condition {
  override def toString(): String = "ne"
}
case class Equal() extends Condition {
  override def toString(): String = "eq"
}
case class LessThan() extends Condition {
  override def toString(): String = "lt"
}
case class GreaterThan() extends Condition {
  override def toString(): String = "gt"
}
case class GreaterOrEqual() extends Condition {
  override def toString(): String = "ge"
}
case class LessOrEqual() extends Condition {
  override def toString(): String = "le"
}
case class Overflow() extends Condition {
  override def toString(): String = "vs"
}
case class NoOverflow() extends Condition {
  override def toString(): String = "vc"
}