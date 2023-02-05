package wacc

sealed trait Identifier

// case class IntIdentifier() extends Identifier

// case class BoolIdentifier() extends Identifier

// case class StrIdentifier() extends Identifier

// case class CharIdentifier() extends Identifier

case class VarIdentifier(ty: String) extends Identifier

case class FuncIdentifier(paramtype: List[String], returntype: String) extends Identifier

//case class ArrayIdentifier(ty: String, size: Int, elements: List[Any]) extends Identifier

case class ArrayIdentifier(ty: String, dim: Int, size: Int, elements: List[Any]) extends Identifier

// val a = newArrayIdentifier("int[]", 3, [1,2,3])
// a.ty == "int[]"