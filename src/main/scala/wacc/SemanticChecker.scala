package wacc

import scala.collection.mutable.Stack

object SemanticChecker {
    var errorMessage = ""
    var symbolTable = new SymbolTable()
    var scope = 0
    var scopeStack = Stack[Int]()

    def check(node: ProgramNode): Unit = {
        val result = node.semanticCheck()
        if (result == "") {
            true
        }
        else {
            result
        }
    }
}