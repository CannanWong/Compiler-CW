package wacc

import scala.collection.mutable.Stack

object SemanticChecker {
    var errorMessage = ""
    var symbolTable = new SymbolTable()
    var nextScope = 0
    var scopeStack = Stack[Int]()

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

    def typeCheck(lhs: TypeNode, rhs: RValueNode): Boolean = {
        /* find type of RHS */
        val lhsType = lhs.typeVal()
        val rhsType = rhs.typeVal()
        val res = (lhsType == rhsType)
        if (!res) {
            errorMessage += "LHS type \"" + lhsType + "\" does not match RHS type \"" + rhsType + "\""
        }
        return res
    }

    def typeCheck(lhs: LValueNode, rhs: RValueNode): Boolean = {
        /* basic types (int, bool, char, string) */
        /* array type */
        /* pair type */
        val res = lhs.typeVal() == rhs.typeVal()
        if (!res) {
            errorMessage += "LHS type \"" + lhs.typeVal() + "\" does not match RHS type \"" + rhs.typeVal() + "\"\n"
        }
        return res
    }
    
    def basicTypeCheck(ty: String, expr: ExprNode): Boolean = {
        val res = ty == expr.typeVal()
        if (expr.typeVal() == "NO TYPE") {
            throw new IllegalArgumentException("Type for EXPR has not been assigned. Please check order of evaluation")
        }
        if (!res) {
           errorMessage += "unexpected type " + expr.typeVal() + ", expected  type " + ty + "\n"
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

    // def arrayTypeCheck(ty: String, expr: ExprNode): Boolean = {
    //     if (expr.typeVal() == "NO TYPE") {
    //         throw new IllegalArgumentException("Type for EXPR has not been assigned. Please check order of evaluation")
    //     }
    //     if (!res) {
    //        errorMessage += "unexpected type " + expr.typeVal() + ", expected  type " + ty + "\n"
    //     }
    //     res       
    // }

    def currScope(): Int = {
        return scopeStack.top
    }
}