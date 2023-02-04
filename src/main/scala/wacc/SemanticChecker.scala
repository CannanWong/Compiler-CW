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
            case IdentNode(name)
                // Check symbol table if variable is of type bool
                => {
                    val varName = "v" + scope.toString() + "!" + name
                    if (SemanticChecker.symbolTable.lookUp(varName).isEmpty) {
                        SemanticChecker.errorMessage += "Semantic error in if statement\n"
                    }
                    else {
                        val identifier = SemanticChecker.symbolTable.lookUp(varName).get
                        identifier match {
                            case VarIdentifier(ty)
                                => if (ty != "bool") {
                                    SemanticChecker.errorMessage += "Semantic error in if statement\n"
                                }
                            case _
                               => SemanticChecker.errorMessage += "[Not possible!] Semantic error in if statement\n" 
                        }
                    }
                }
            case ArrayElemNode(IdentNode(name), exprList)
                // Check symbol table if array elem is of type bool
                => {
                    val arrayName = "a" + scope.toString() + "!" + name
                    if (SemanticChecker.symbolTable.lookUp(arrayName).isEmpty) {
                        SemanticChecker.errorMessage += "Semantic error in if statement\n"
                    }
                    else {
                        val identifier = SemanticChecker.symbolTable.lookUp(arrayName).get
                        identifier match {
                            case ArrayIdentifier(ty, _, _)
                                => if (exprList.length == 1) {
                                    if (ty != "bool") {
                                        SemanticChecker.errorMessage += "Semantic error in if statement\n"
                                    }
                                }
                                // else {2+ dimension arrays}
                            case _
                               => SemanticChecker.errorMessage += "[Not possible!] Semantic error in if statement\n" 
                        }
                    }
                }
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