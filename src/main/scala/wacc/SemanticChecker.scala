package wacc

import scala.collection.mutable.Stack

object SemanticChecker {
    var errorMessage = ""
    var symbolTable = new SymbolTable()
    var scope = 0
    var scopeStack = Stack[Int]()

    def check(node: ProgramNode): String = {
        node.semanticCheck()
        errorMessage
    }

    def checkIfWhileCond(expr: ExprNode): Unit = {
        expr match {
            case IntLiterNode(_) | CharLiterNode(_) | StrLiterNode(_) | PairLiterNode()
                => SemanticChecker.errorMessage += "Semantic error in if statement\n"
            case IdentNode(_)
                // Check symbol table if variable is of type bool
                => SemanticChecker.errorMessage += "Semantic error in if statement\n"
            case ArrayElemNode(_,_)
                // Check symbol table if array elem is of type bool
                => SemanticChecker.errorMessage += "Semantic error in if statement\n"
            case UnOpExprNode(UnaryOperNode(op),_)
                => if (op != "!") {
                        SemanticChecker.errorMessage += "Semantic error in if statement\n"
                    }
            case BinOpExprNode(_,BinaryOperatorNode(op),_)
                => if (op == "*" || op == "/" || op == "%" || op == "+" || op == "-") {
                        SemanticChecker.errorMessage += "Semantic error in if statement\n"
                    }
            case _ =>
        }
    }
}