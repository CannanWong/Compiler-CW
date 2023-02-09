package wacc

sealed trait Identifier

case class FuncIdentifier(paramtype: List[String], returntype: String) extends Identifier

// case class IntIdentifier() extends Identifier

// case class BoolIdentifier() extends Identifier

// case class StrIdentifier() extends Identifier

// case class CharIdentifier() extends Identifier

case class VarIdentifier(ty: String) extends Identifier

case class ArrayIdentifier(ty: String, dim: Int) extends Identifier

case class PairIdentifier(ty1: String, ty2: String) extends Identifier