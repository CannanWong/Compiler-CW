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

    def validDeclaration(node: IdentNode): Boolean = {
        val varName = currScope().toString() + "!" + node.name
        symbolTable.lookUp(varName) match {
            case Some(n) => {
                errorMessage += "variable name \"" + node.name + "\" is already used in the same scope"
                return false
            }
            case _ => true
        }
    }

    def findType(lhs: LValueNode) : String = {
        return ""
    }

    def findType(rhs: RValueNode) : String = {
        return rhs match {
            /* basic type literals (int, bool, char, string) */
            case i: IntLiterNode => "int"
            case b: BoolLiterNode => "bool"
            case c: CharLiterNode => "char"
            case s: StrLiterNode => "str"
            /* identifier (var and func) */
            case id: IdentNode => {
                val varName = currScope().toString() + "!" + id.name
                val res = symbolTable.lookUp(varName)
                res match {
                    case Some(identif) => {
                        identif match {
                            case VarIdentifier(ty) => ty
                            case FuncIdentifier(paramtype, returntype) => returntype
                            case ArrayIdentifier(ty, dim, size, elements) => ty + ":" + dim
                        }
                    }
                    case None => {
                        errorMessage += "variable name \"" + id.name + "\" is is not defined"
                        println("var not defined, function should return false here")
                        return "ERROR"
                    }
                }
            }
            case _ => ???
        }
    }

    def findType(ty: TypeNode) : String = {
        ty match {
            case BaseTypeNode(t) => t
            case a: ArrayTypeNode => a.typeVal
            case p: PairTypeNode => p.typeVal
        }
    }

    def typeCheck(lhs: TypeNode, rhs: RValueNode): Boolean = {
        /* find type of RHS */
        val lhsType = findType(lhs)
        val rhsType = findType(rhs)
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
        return true
    }

    def currScope(): Int = {
        return scopeStack.top
    }

    def checkIfWhileCond(expr: ExprNode): Unit = {
        expr match {
            case IntLiterNode(_) | CharLiterNode(_) | StrLiterNode(_) | PairLiterNode()
                => errorMessage += "Semantic error in if statement\n"
            case IdentNode(name)
                // Check symbol table if variable is of type bool
                => {
                    val varName = currScope().toString() + "!" + name
                    if (symbolTable.lookUp(varName).isEmpty) {
                        errorMessage += "Semantic error in if statement\n"
                    }
                    else {
                        val identifier = symbolTable.lookUp(varName).get
                        identifier match {
                            case VarIdentifier(ty)
                                => if (ty != "bool") {
                                    errorMessage += "Semantic error in if statement\n"
                                }
                            case _
                               => errorMessage += "[Not possible!] Semantic error in if statement\n" 
                        }
                    }
                }
            case ArrayElemNode(IdentNode(name), exprList)
                // Check symbol table if array elem is of type bool
                => {
                    val arrayName = currScope().toString() + "!" + name
                    if (symbolTable.lookUp(arrayName).isEmpty) {
                        errorMessage += "Semantic error in if statement\n"
                    }
                    else {
                        val identifier = symbolTable.lookUp(arrayName).get
                        identifier match {
                            case ArrayIdentifier(ty,_, _, _)
                                => if (exprList.length == 1) {
                                    if (ty != "bool") {
                                        errorMessage += "Semantic error in if statement\n"
                                    }
                                }
                                // else {2+ dimension arrays}
                            case _
                               => errorMessage += "[Not possible!] Semantic error in if statement\n" 
                        }
                    }
                }
            case UnOpExprNode(UnaryOperNode(op),_)
                => if (op != "!") {
                        errorMessage += "Semantic error in if statement\n"
                    }
            case BinOpExprNode(_,BinaryOperatorNode(op),_)
                => if (op == "*" || op == "/" || op == "%" || op == "+" || op == "-") {
                        errorMessage += "Semantic error in if statement\n"
                    }
            case _ =>
        }
    }
}