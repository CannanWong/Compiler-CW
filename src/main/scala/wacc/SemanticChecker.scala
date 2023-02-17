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
    def TypeCheck(ty: TypeNode, rvalue: RValueNode): Boolean = {
       ty.TypeVal().typeEquals(rvalue.TypeVal())
    }

    def TypeCheck(ty1: Identifier, ty2: Identifier): Boolean = {
        ty1.typeEquals(ty2)
    }

    // LValuesAssignNode
    def TypeCheck(lhs: LValueNode, rvalue: RValueNode): Boolean = {
        lhs.TypeVal().typeEquals(rvalue.TypeVal()) 
    }

    def TypeCheck(ty: Identifier, rvalue: RValueNode): Boolean = {
        val rhsType = rvalue.TypeVal()
        ty.typeEquals(rhsType)
    }

    def currScope(): Int = {
        return scopeStack.top
    }
}