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
        scopeStack.push(0)
        nextScope = 1
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
                typeCheckArray(ty.arrayType(), rvalue.arrayType(), ty.arrayDim(), rvalue.arrayDim())
            }
            else if (lhsType == "pair") {
                typeCheckPair(ty.fstType(), rvalue.fstType(), ty.sndType(), rvalue.sndType())
            }
            if (lhsType == "any") {
                errorMessage += "Undefined type in declaration\n"
            }
        }
        else if (!(lhsType == "pair" && rhsType == "null" || lhsType == "any" ||
                 rhsType == "any")) {
            errorMessage +=
                "Wrong type in declaration, expected " + lhsType + " instead of " + rhsType + "\n"
        }
    }

    def typeCheck(lvalue: LValueNode, rvalue: RValueNode): Unit = {
        val lhsType = lvalue.typeVal()
        val rhsType = rvalue.typeVal()
        if (lhsType == rhsType) {
            if (lhsType == "array") {
                typeCheckArray(lvalue.arrayType(), rvalue.arrayType(),
                                lvalue.arrayDim(), rvalue.arrayDim())
            }
            else if (lhsType == "pair") {
                typeCheckPair(lvalue.fstType(), rvalue.fstType(), lvalue.sndType(), rvalue.sndType())
            }
            if (lhsType == "any") {
                errorMessage += "Undefined type in assignment\n"
            }
        }
        else if (!(lhsType == "pair" && rhsType == "null" || lhsType == "any" || rhsType == "any")) {
            errorMessage += "Wrong type in assignment, expected " +
                                lhsType + " instead of " + rhsType + "\n"
        }
    }

    def typeCheckArray(lhsArrayType: String, rhsArrayType: String,
                        lhsArrayDim: Int, rhsArrayDim: Int) {
        if (lhsArrayType != rhsArrayType && rhsArrayType != "any") {
            errorMessage += s"array assignment type error: expect: " +
                            "${lhsArrayType}, provided: ${rhsArrayType}\n"
        }
        if (lhsArrayDim != rhsArrayDim) {
            errorMessage += "Wrong dimension in array assignment\n"
        }
    }

    def typeCheckPair(lhsFstType: String, rhsFstType: String,
                            lhsSndType: String, rhsSndType: String) {
        if (lhsFstType != "null" && rhsFstType != "null" &&
            lhsFstType != "any" && rhsFstType != "any") {
            if (lhsFstType != rhsFstType) {
                errorMessage += "Wrong type in pair declaration, expected " +
                                lhsFstType + " instead of " + rhsFstType + "\n"
            }
        }
        if (lhsSndType != "null" && rhsSndType != "null" && 
            lhsSndType != "any" && rhsSndType != "any") {
            if (lhsSndType != rhsSndType) {
                errorMessage += "Wrong type in pair declaration, expected " +
                                lhsSndType + " instead of " + rhsSndType + "\n"
            }
        }
        if (lhsFstType == "any" && rhsFstType == "any" ||
                 lhsSndType == "any" && rhsSndType == "any") {
            errorMessage += "Undefined type in assignment\n"
        }
    
    }
    
    def basicTypeCheck(ty: String, expr: ExprNode): Unit = {
        if (expr.typeVal() == "any") {
            true
        } else if (expr.typeVal() == "null") {
            false
        }
        if (ty != expr.typeVal()) {
            errorMessage += s"unexpected type ${expr.typeVal()}, expected type ${ty}\n"
        }     
    }

    def basicTypeCheck(ty1: String, ty2: String, expr: ExprNode): Unit = {
        val res = ty1 == expr.typeVal() || ty2 == expr.typeVal() || expr.typeVal() == "any"
        if (!res) {
           errorMessage += s"unexpected type ${expr.typeVal()}, expected type ${ty1} / ${ty2}\n"
        }      
    }

    def currScope(): Int = {
        return scopeStack.top
    }
}