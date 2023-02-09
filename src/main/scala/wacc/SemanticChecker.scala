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

    // Check if symbol table contains variable in current or higher scopes
    def tableContainsIdentifier(id: IdentNode): Boolean = {
        if (symbolTable.lookUpVar(id.name) == None) {
            errorMessage += "variable name \"" + id.name + "\" is not defined in this scope\n"
            false
        }
        true
    }

    def typeCheck(ty: TypeNode, rvalue: RValueNode): Unit = {
        val lhsType = ty.typeVal()
        val rhsType = rvalue.typeVal()
        if (lhsType == rhsType) {
            if (lhsType == "array") {
                if (ty.arrayType() != rvalue.arrayType() && rvalue.arrayType() != "any") {
                    errorMessage += "Wrong type in array declaration\n"
                }
                if (ty.arrayDim() != rvalue.arrayDim()) {
                    errorMessage += "Wrong dimension in array declaration\n"
                }
            }
            else if (lhsType == "pair") {
                if (ty.fstType() != rvalue.fstType() || ty.sndType() != rvalue.sndType()) {
                    errorMessage += "Wrong type in pair declaration\n"
                }
            }
        }
        else if (!(lhsType == "pair" && rhsType == "null")) {
            errorMessage += "Wrong type in declaration\n"
        }
    }

    def typeCheck(lvalue: LValueNode, rvalue: RValueNode): Unit = {
        val lhsType = lvalue.typeVal()
        val rhsType = rvalue.typeVal()
        if (lhsType == rhsType) {
            if (lhsType == "array") {
                if (lvalue.arrayType() != rvalue.arrayType() && rvalue.arrayType() != "any") {
                    errorMessage += "Wrong type in array declaration\n"
                }
                if (lvalue.arrayDim() != rvalue.arrayDim()) {
                    errorMessage += "Wrong dimension in array declaration\n"
                }
            }
            else if (lhsType == "pair") {
                if (lvalue.fstType() != rvalue.fstType() || lvalue.sndType() != rvalue.sndType()) {
                    errorMessage += "Wrong type in pair declaration\n"
                }
            }
        }
        else {
            errorMessage += "Wrong type in declaration\n"
        }
    }
    
    def basicTypeCheck(ty: String, expr: ExprNode): Boolean = {
        val res = ty == expr.typeVal()
        if (expr.typeVal() == "NO TYPE") {
            throw new IllegalArgumentException("Type for EXPR has not been assigned. Please check order of evaluation")
        }
        if (!res) {
           errorMessage += "unexpected type " + expr.typeVal() + ", expected type " + ty + "\n"
        }
        res       
    }

    def basicTypeCheck(ty1: String, ty2: String, expr: ExprNode): Boolean = {
        val res = ty1 == expr.typeVal() || ty2 == expr.typeVal()
        if (expr.typeVal() == "NO TYPE") {
            throw new IllegalArgumentException("Type for EXPR has not been assigned. Please check order of evaluation")
        }
        if (!res) {
           errorMessage += s"unexpected type ${expr.typeVal()}, expected type ${ty1} / ${ty2}\n"
        }
        res       
    }

    def currScope(): Int = {
        return scopeStack.top
    }
}