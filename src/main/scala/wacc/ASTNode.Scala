package wacc

class ProgramNode(funcList: List[FuncNode], stat: StatNode) 

class FuncNode(type: TypeNode, ident: IdentNode, paramList: paramListNode, stat: StatNode)

class StatNode()

class skipNode() extends StatNode

class LValueNode(ident: IdentNode, arrayElem: ArrayElemNode, pairElem: PairElemNode)

class PairElemNode(fstLValue: LValueNode, sndLValue: LValueNode)

class RValueNode(ident: IdentNode, arrayElem: ArrayElemNode, pairElem: PairElemNode)