package wacc

sealed trait Identifier

case class VarIdentifier(ty: String) extends Identifier

case class FuncIdntifier(paramtype: List[String], returntype: String) extends Identifier

case class ArrayIdentifier(ty: String, size: Int, elements: List[Any]) extends Identifier

// val a = newArrayIdentifier("int[]", 3, [1,2,3])
// a.ty == "int[]"