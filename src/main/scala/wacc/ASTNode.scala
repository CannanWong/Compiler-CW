package wacc

//case class ProgramNode(funcList: List[FuncNode], stat: StatNode)
case class ProgramNode(funcList: List[FuncNode], stat: StatNode) 

case class FuncNode(ty: TypeNode, ident: IdentNode, paramList: ParamListNode, stat: StatNode)

case class ParamListNode(paramList: List[ParamNode])

case class ParamNode(ty: TypeNode, ident: IdentNode)

// StatNode
sealed trait StatNode

case class SkipNode() extends StatNode

case class AssignIdentNode(ty: TypeNode, ident: IdentNode, rvalue: RValueNode) extends StatNode

case class ValuesEqualNode(lvalue: LValueNode, rvalue: RValueNode) extends StatNode

case class ReadNode(lvalue: LValueNode) extends StatNode

case class FreeNode(expr: ExprNode) extends StatNode

case class ReturnNode(expr: ExprNode) extends StatNode

case class ExitNode(expr: ExprNode) extends StatNode

case class PrintNode(expr: ExprNode) extends StatNode

case class PrintlnNode(expr: ExprNode) extends StatNode

case class IfNode(expr: ExprNode, fstStat: StatNode, sndStat: StatNode) extends StatNode

case class WhileNode(expr: ExprNode, stat: StatNode) extends StatNode

case class BeginEndNode(stat: StatNode) extends StatNode

case class StatJoinNode(statList: List[StatNode]) extends StatNode

// LValueNode
sealed trait LValueNode

case class IdentNode(name: String) extends LValueNode with ExprNode

case class ArrayElemNode(ident: IdentNode, exprList: List[ExprNode]) extends LValueNode with ExprNode

sealed trait PairElemNode extends LValueNode with RValueNode

case class FstNode(expr: LValueNode) extends PairElemNode 

case class SndNode(expr: LValueNode) extends PairElemNode 

// RValueNode
sealed trait RValueNode

sealed trait ExprNode extends RValueNode

case class ArrayLiterNode(exprList: List[ExprNode]) extends ExprNode

case class NewPairNode(fstExpr: ExprNode, sndExpr: ExprNode) extends RValueNode

case class CallNode(ident: IdentNode, argList: ArgListNode) extends RValueNode 

case class ArgListNode(exprList: List[ExprNode])

sealed trait TypeNode

case class BaseTypeNode(ty: String) extends TypeNode with PairElemTypeNode

case class ArrayTypeNode(ty: TypeNode) extends TypeNode with PairElemTypeNode

case class PairTypeNode(fstPET: PairElemTypeNode, sndPET: PairElemTypeNode) extends TypeNode

// PairElemTypeNode
sealed trait PairElemTypeNode

case class PETPairNode() extends PairElemTypeNode

case class IntLiterNode(n: Int) extends ExprNode

case class BoolLiterNode(b: Boolean) extends ExprNode

case class CharLiterNode(c: Char) extends ExprNode

case class StrLiterNode(s: String) extends ExprNode

case class PairLiterNode() extends ExprNode

case class UnOpExprNode(op: UnaryOperNode, expr: ExprNode) extends ExprNode

case class BinOpExprNode(fstexpr: ExprNode, op: BinaryOperatorNode, sndexpr: ExprNode) extends ExprNode

case class BracketExprNode(expr: ExprNode) extends ExprNode

case class UnaryOperNode(op: String)

case class BinaryOperatorNode(op: String)