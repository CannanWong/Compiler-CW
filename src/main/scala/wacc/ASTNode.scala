package wacc

import scala.collection.mutable.ListBuffer

sealed trait ASTNode {
    def semanticCheck(): Unit = {
    }
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
    
    /**
      * Checks for:
        1. legal params
        2. legal stats
        3. legal type (tbc)
        4. leagal func ident
        5. return type matches function type
      */

    // ! Check all execution paths contain return or exit
    override def semanticCheck(): Unit = {
        var paramtypeList = ListBuffer[String]()
        for (param <- paramList.paramList) {
            // Check for repeated parameter names
            if (SemanticChecker.validDeclaration(param.ident)) {
                paramtypeList += param.ty.typeVal
                SemanticChecker.symbolTable.addVar(param.ident.name, param.ty.typeVal)
            }
        }

        // Check ident
        if (SemanticChecker.validDeclaration(ident)) {
            // Add to symbol table
            SemanticChecker.symbolTable.addFunc(ident.name, paramtypeList.toList, ty.typeVal)
        }
        
        // Check return type
        stat match {
            case s: StatJoinNode
                => { 
                    for (stat <- s.statList) {
                        stat match {
                            case ReturnNode(expr) 
                                => SemanticChecker.typeCheck(ty, expr)
                            case _ => // ! Find all return nodes
                        }
                    }
                }
            case _ =>
        }

        ty.semanticCheck()
        // ident.semanticCheck()
        // paramList.semanticCheck()

        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        stat.semanticCheck()
        SemanticChecker.scopeStack.pop()
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

    /**
     * Checks for:
       1. ident's name not declared in scope
       2. RHS val and type consistent 
       3. rval is semantically correct (TODO)
       4. lhs type is not func (type check should catch)
      */

    override def semanticCheck(): Unit = {
        SemanticChecker.validDeclaration(ident)
        rvalue.semanticCheck()
        if (SemanticChecker.validDeclaration(ident) && SemanticChecker.typeCheck(ty, rvalue)) {
            SemanticChecker.symbolTable.addVar(ident.name, SemanticChecker.findTypeR(rvalue))
        }
    }
}

case class LValuesAssignNode(lvalue: LValueNode, rvalue: RValueNode) extends StatNode {
    /**
      * check for:
        1. reassignment to itself (TODO pairs)
        2. lhs type is not func (type check should catch)
      */
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        rvalue.semanticCheck()

        var lname = ""
        var rname = ""

        var bothDefined = true

        /* check that ident(left) is not asssigned to itself */
        val lValTableHName = lvalue match{
            case id: IdentNode => {
                val varName = SemanticChecker.currScope().toString() + "!" + id.name
                lname = id.name
                if (SemanticChecker.tableContainsIdentifier(id)) {
                    varName
                } else {
                    bothDefined = false
                    "NO SUCH THING AS -" + SemanticChecker.currScope() + "!" + id.name
                }
            }              
            case _ => "ARRAY: should pass test PAIR: TODO"
        }

        val rValTableHName = rvalue match{
            case id: IdentNode => {
                val varName = SemanticChecker.currScope().toString() + "!" + id.name
                rname = id.name
                if (SemanticChecker.tableContainsIdentifier(id)) {
                    varName
                } else {
                    bothDefined = false
                    "NO SUCH THING AS -" + SemanticChecker.currScope() + "!" + id.name
                }
            }     
            case _ => "ARRAY: should pass test PAIR: TODO"
        }

        if (lValTableHName == rValTableHName) {
            SemanticChecker.errorMessage += "Reassignment to same variable: " + lname + " to " + rname + "\n"
        }     
        else if (bothDefined) {
            if (SemanticChecker.typeCheck(lvalue, rvalue)) {
                SemanticChecker.errorMessage += "successful assignment of variable: " + lValTableHName + " to " + rValTableHName + "\n"
            }
        }
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
        SemanticChecker.checkIfWhileCond(expr)
        expr.semanticCheck()

        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        fstStat.semanticCheck()
        SemanticChecker.scopeStack.pop()

        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        sndStat.semanticCheck()
        SemanticChecker.scopeStack.pop()
    }
}

case class WhileNode(expr: ExprNode, stat: StatNode) extends StatNode  {
    override def semanticCheck(): Unit = {
        SemanticChecker.checkIfWhileCond(expr)
        expr.semanticCheck()
        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        stat.semanticCheck()
        SemanticChecker.scopeStack.pop()
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

case class IdentNode(name: String) extends LValueNode with ExprNode { 
    // ! To be deleted !

    // symbol table name is decided during traversal of nodes to do semantic checks
    // var scope = 0
    // var symbolTableName = "PLACEHODER: " + name + " : " + scope

    // override def semanticCheck(): Unit = {
    //     SemanticChecker.validDeclaration(this)
    // }
}

case class ArrayElemNode(ident: IdentNode, exprList: List[ExprNode]) extends LValueNode with ExprNode {
    override def semanticCheck(): Unit = {
        SemanticChecker.validDeclaration(ident)
        ident.semanticCheck()
        for (e <- exprList) {
            e.semanticCheck()
        }
    }
}

sealed trait PairElemNode extends LValueNode with RValueNode

case class FstNode(lvalue: LValueNode) extends PairElemNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
    }
}

case class SndNode(lvalue: LValueNode) extends PairElemNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
    }
}

// RValueNode
sealed trait RValueNode extends ASTNode

sealed trait ExprNode extends RValueNode

case class ArrayLiterNode(exprList: List[ExprNode]) extends ExprNode {
    override def semanticCheck(): Unit = {
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
        // Check if function is declared
        val lookUp = SemanticChecker.symbolTable.lookUpFunc(ident.name)
        if (lookUp == None) {
            SemanticChecker.errorMessage += "Function name \"" + ident.name + "\" is not defined in this scope\n"
        }
        // Check if arg list is of correct type and number
        else {
            val identifier = lookUp.get
            identifier match {
                case f: FuncIdentifier
                    => {
                        if (argList.exprList.length != f.paramtype.length) {
                            SemanticChecker.errorMessage += "Function argument number is different from function parameter number\n"
                        }
                        else {
                            var index = 0
                            for (arg <- argList.exprList) {
                                if (SemanticChecker.findTypeR(arg) != f.paramtype.apply(index)) {
                                    SemanticChecker.errorMessage += "Function argument type is incorrect\n"
                                }
                                index += 1
                            }
                        }
                    }
                case _ => SemanticChecker.errorMessage += "[Not possible!]"
            }
        }
        // ident.semanticCheck()
        argList.semanticCheck()
    }
}


case class ArgListNode(exprList: List[ExprNode]) extends ASTNode {
    override def semanticCheck(): Unit = {
        for (e <- exprList) {
            e.semanticCheck()
        }
    }
}

sealed trait TypeNode extends ASTNode {
    val typeVal: String
}

case class BaseTypeNode(ty: String) extends TypeNode with PairElemTypeNode {
    val typeVal = ty
}

case class ArrayTypeNode(ty: TypeNode) extends TypeNode with PairElemTypeNode {
    override def semanticCheck(): Unit = {
        ty.semanticCheck()
    }
    def countDimension(ty: TypeNode): Int = {
        ty match {
            case ArrayTypeNode(arrayTy) => 1 + countDimension(arrayTy)
            case node => return 1
        }  
    }

    def getType(ty: TypeNode): TypeNode = {
        ty match {
            case ArrayTypeNode(arrayTy) => getType(arrayTy)
            case node => return node
        }  
    }
    val dimension = countDimension(ty)
    val baseType = getType(ty)
    val typeVal = baseType.typeVal + ":" + dimension
}

case class PairTypeNode(fstPET: PairElemTypeNode, sndPET: PairElemTypeNode) extends TypeNode {
    val typeVal = "pair"
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
    val evalType = op match {
        case BinaryOperatorNode("") | BinaryOperatorNode("") | BinaryOperatorNode("")| BinaryOperatorNode("") | BinaryOperatorNode("")
                => "int"
        case _ => "boolean"
    }

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