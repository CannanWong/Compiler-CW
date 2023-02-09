package wacc

import scala.collection.mutable.Stack

object SemanticChecker {
    var errorMessage = ""
    var symbolTable = new SymbolTable()
    var nextScope = 0
    var scopeStack = Stack[Int]()
    var insideFunc = true

    def check(node: ProgramNode): String = {
        resetSemanticChecker()
        node.semanticCheck()
        errorMessage
    }

    /* for running tests */
    def resetSemanticChecker(): Unit = {
        errorMessage = ""
        symbolTable = new SymbolTable()
        nextScope = 0
        scopeStack.push(0)
    }

    def tableContainsIdentifier(id :IdentNode): Boolean = {
        if (symbolTable.lookUpVar(id.name) == None) {
            errorMessage += "variable name \"" + id.name + "\" is is not defined in this scope\n"
            return false
        }
        return true
    }

    // for AssignIdentNode
    def typeCheck1(ty: TypeNode, rvalue: RValueNode): Boolean = {
       ty.typeVal1().typeEquals(rvalue.typeVal1())
    }

    // LValuesAssignNode
    def typeCheck1(lhs: LValueNode, rvalue: RValueNode): Boolean = {
        lhs.typeVal1().typeEquals(rvalue.typeVal1())
    }

    def typeCheck1(ty: Identifier, rvalue: RValueNode): Boolean = {
        val rhsType = rvalue.typeVal1()
        ty.typeEquals(rhsType)
    }

    def currScope(): Int = {
        return scopeStack.top
    }
}