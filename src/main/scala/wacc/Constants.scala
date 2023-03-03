package wacc

import wacc.FixedRegister
import wacc.ImmVal

object  Constants {
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

  val immTrue = ImmVal(1)
  val immFalse = ImmVal(0)

  // TODO: maybe constants not negative
  val ARRAY_LENGTH_OFFSET: Int = -4
  val INT_SIZE = -4
  val BYTE_SIZE = 1
  
  def data_offset(dataType :Int): Int = {
    dataType * -1
  }

  val INT_HIGHEST_BIT = 31

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
  val NOT_EQUAL = "ne"
  val EQUAL = "eq"
  val LESS_THAN = "lt"
  val GREATER_THAN = "gt"
  val GREATER_OR_EQUAL = "ge"
  val LESS_OR_EQUAL = "le"
  val OVERFLOW = "vs"
  val NO_OVERFLOW = "vc"

  val ESCAPE_CHAR_LIST = List[String](s"{\"}")
}
