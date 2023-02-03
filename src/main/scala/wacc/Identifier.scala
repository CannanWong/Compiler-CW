package wacc

sealed trait Identifier

case class VarIdentifier(ty: String)

case class FuncIdntifier(paramtype: List[String], returntype: String)

case class ArrayIdentifier(ty: String, size: Int, elements: List[Any])

// val a = newArrayIdentifier("int[]", 3, [1,2,3])
// a.ty == "int[]"