package wacc

import scala.collection.mutable.Stack

object SemanticChecker {
    var symbolTable = new SymbolTable()
    var nextScope = 0
    var scopeStack = Stack[Int]()
    var insideFunc = true

    def check(node: ProgramNode): Unit = {
        resetSemanticChecker()
        node.semanticCheck()
    }

    /* for running tests */
    def resetSemanticChecker(): Unit = {
        symbolTable = new SymbolTable()
        scopeStack.push(0)
        nextScope = 1
    }

    // Check if symbol table contains variable in current or higher scopes
    def tableContainsIdentifier(id: IdentNode): Boolean = {
        if (symbolTable.lookUpVar(id.name) == None) {
              Error.addSemErr("variable name \"" + id.name + "\" is not defined in this scope")
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
                Error.addSemErr("Undefined type in declaration")
            }
        }
        else if (!(lhsType == "pair" && rhsType == "null" || lhsType == "any" ||
                 rhsType == "any")) {
            Error.addSemErr(
                "Wrong type in declaration, expected " + lhsType + " instead of " + rhsType)
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
                Error.addSemErr("Undefined type in assignment")
            }
        }
        else if (!(lhsType == "pair" && rhsType == "null" || lhsType == "any" || rhsType == "any")) {
            Error.addSemErr("Wrong type in assignment, expected " +
                                lhsType + " instead of " + rhsType)
        }
    }

    def typeCheckArray(lhsArrayType: String, rhsArrayType: String,
                        lhsArrayDim: Int, rhsArrayDim: Int): Unit = {
        if (lhsArrayType != rhsArrayType && rhsArrayType != "any") {
            Error.addSemErr(s"array assignment type error: expect: " +
                            s"${lhsArrayType}, provided: ${rhsArrayType}")
        }
        if (lhsArrayDim != rhsArrayDim) {
            Error.addSemErr("Wrong dimension in array assignment")
        }
    }

    def typeCheckPair(lhsFstType: String, rhsFstType: String,
                            lhsSndType: String, rhsSndType: String): Unit = {

        if (lhsFstType != "null" && rhsFstType != "null" &&
            lhsFstType != "any" && rhsFstType != "any") {
            if (lhsFstType != rhsFstType) {
                Error.addSemErr("Wrong type in pair declaration, expected " +
                                lhsFstType + " instead of " + rhsFstType)
            }
        }
        if (lhsSndType != "null" && rhsSndType != "null" && 
            lhsSndType != "any" && rhsSndType != "any") {
            if (lhsSndType != rhsSndType) {
                Error.addSemErr("Wrong type in pair declaration, expected " +
                                lhsSndType + " instead of " + rhsSndType)
            }
        }
        if (lhsFstType == "any" && rhsFstType == "any" ||
                 lhsSndType == "any" && rhsSndType == "any") {
            Error.addSemErr("Undefined type in assignment")
        }
    
    }
    
    def basicTypeCheck(ty: String, expr: ExprNode): Unit = {
        if (expr.typeVal() == "any") {
            true
        } else if (expr.typeVal() == "null") {
            false
        }
        if (ty != expr.typeVal()) {
            Error.addSemErr(s"unexpected type ${expr.typeVal()}, expected type ${ty}")
        }     
    }

    def basicTypeCheck(ty1: String, ty2: String, expr: ExprNode): Unit = {
        val res = ty1 == expr.typeVal() || ty2 == expr.typeVal() || expr.typeVal() == "any"
        if (!res) {
           Error.addSemErr(s"unexpected type ${expr.typeVal()}, expected type ${ty1} / ${ty2}")
        }      
    }

    def currScope(): Int = {
        return scopeStack.top
    }
}