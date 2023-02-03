package wacc

sealed trait ASTNode {
    def semanticCheck(): Unit
    var errorMessage = ""
}

case class ProgramNode(funcList: List[FuncNode], stat: StatNode) extends ASTNode {
    override def semanticCheck(): Unit = {
        for (f <- funcList) {
            f.semanticCheck()
        }
        stat.semanticCheck()
    }
}

case class FuncNode(ty: TypeNode, ident: IdentNode, paramList: ParamListNode, stat: StatNode) extends ASTNode {
    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        ident.semanticCheck()
        paramList.semanticCheck()
        stat.semanticCheck()
    }
}

case class ParamListNode(paramList: List[ParamNode]) extends ASTNode {
    override def semanticCheck(): Unit = {
        for (p <- paramList) {
            p.semanticCheck()
        }
    }
}

case class ParamNode(ty: TypeNode, ident: IdentNode) extends ASTNode {
    ty.semanticCheck()
    ident.semanticCheck()
}

// StatNode
sealed trait StatNode extends ASTNode

case class SkipNode() extends StatNode

case class AssignIdentNode(ty: TypeNode, ident: IdentNode, rvalue: RValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        ident.semanticCheck()
        rvalue.semanticCheck()
    }
}

case class LValuesAssignNode(lvalue: LValueNode, rvalue: RValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        rvalue.semanticCheck()
    }
}

case class ReadNode(lvalue: LValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
    }
}

case class FreeNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
    }
}

case class ReturnNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
    }
}

case class ExitNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
    }
}

case class PrintNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
    }
}

case class PrintlnNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
    }
}

case class IfNode(expr: ExprNode, fstStat: StatNode, sndStat: StatNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        fstStat.semanticCheck()
        sndStat.semanticCheck()
    }
}

case class WhileNode(expr: ExprNode, stat: StatNode) extends StatNode  {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        stat.semanticCheck()
    }
}

case class BeginEndNode(stat: StatNode) extends StatNode  {
    override def semanticCheck(): Unit = {
        stat.semanticCheck()
    }
}

case class StatJoinNode(statList: List[StatNode]) extends StatNode  {
    override def semanticCheck(): Unit = {
        for (s <- statList) {
            s.semanticCheck()
        }
    }
}

// LValueNode
sealed trait LValueNode extends ASTNode

case class IdentNode(name: String) extends LValueNode with ExprNode 

case class ArrayElemNode(ident: IdentNode, exprList: List[ExprNode]) extends LValueNode with ExprNode {
    override def semanticCheck(): Unit = {
        ident.semanticCheck()
        for (e <- exprList) {
            e.semanticCheck()
        }
    }
}

sealed trait PairElemNode extends LValueNode with RValueNode

case class FstNode(expr: ExprNode) extends PairElemNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
    }
}

case class SndNode(expr: ExprNode) extends PairElemNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
    }
}

// RValueNode
sealed trait RValueNode extends ASTNode

sealed trait ExprNode extends RValueNode

case class ArrayLiterNode(expr: ExprNode, exprList: List[ExprNode]) extends ExprNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        for (e <- exprList) {
            e.semanticCheck()
        }
    }
}

case class NewPairNode(fstExpr: ExprNode, sndExpr: ExprNode) extends RValueNode {
    override def semanticCheck(): Unit = {
        fstExpr.semanticCheck()
        sndExpr.semanticCheck()
    }
}


case class CallNode(ident: IdentNode, argList: ArgListNode) extends RValueNode {
    override def semanticCheck(): Unit = {
        ident.semanticCheck()
        argList.semanticCheck()
    }
}


case class ArgListNode(exprList: List[ExprNode]) {
    override def semanticCheck(): Unit = {
        for (e <- exprList) {
            e.semanticCheck()
        }
    }
}

sealed trait TypeNode extends ASTNode

case class BaseTypeNode(ty: String) extends TypeNode with PairElemTypeNode

case class ArrayTypeNode(ty: TypeNode) extends TypeNode with PairElemTypeNode

case class PairTypeNode(fstPET: PairElemTypeNode, sndPET: PairElemTypeNode) extends TypeNode {
    override def semanticCheck(): Unit = {
        fstPET.semanticCheck()
        sndPET.semanticCheck()
    }
}

// PairElemTypeNode
sealed trait PairElemTypeNode extends ASTNode

case class PETPairNode() extends PairElemTypeNode

case class IntLiterNode(n: Int) extends ExprNode

case class BoolLiterNode(b: Boolean) extends ExprNode

case class CharLiterNode(c: Char) extends ExprNode

case class StrLiterNode(s: String) extends ExprNode

case class PairLiterNode() extends ExprNode

case class UnOpExprNode(op: UnaryOperNode, expr: ExprNode) extends ExprNode {
    override def semanticCheck(): Unit = {
        op.semanticCheck()
        expr.semanticCheck()
    }
}

case class BinOpExprNode(fstExpr: ExprNode, op: BinaryOperatorNode, sndExpr: ExprNode) extends ExprNode {
    override def semanticCheck(): Unit = {
        fstExpr.semanticCheck()
        op.semanticCheck()
        sndExpr.semanticCheck()
    }
}

case class BracketExprNode(expr: ExprNode) extends ExprNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
    }
}

case class UnaryOperNode(op: String) extends ASTNode

case class BinaryOperatorNode(op: String) extends ASTNode