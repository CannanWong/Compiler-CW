package wacc

//class ProgramNode(funcList: List[FuncNode], stat: StatNode) 
case class ProgramNode(stat:StatNode)

//class FuncNode(t: TypeNode, ident: IdentNode, paramList: paramListNode, stat: StatNode)

sealed trait StatNode

case class SkipNode() extends StatNode

case class StatJoinNode(stats: List[StatNode]) extends StatNode

case class IfNode() extends StatNode

//class LValueNode(ident: IdentNode, arrayElem: ArrayElemNode, pairElem: PairElemNode)

//class PairElemNode(fstLValue: LValueNode, sndLValue: LValueNode)

//class RValueNode(ident: IdentNode, arrayElem: ArrayElemNode, pairElem: PairElemNode)