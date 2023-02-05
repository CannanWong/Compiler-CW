package wacc

import scala.collection.mutable.Stack

object SemanticChecker {
    var errorMessage = ""
    var symbolTable = new SymbolTable()
    var scope = 0
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
        scope = 0
        scopeStack = Stack[Int]()
        scopeStack.push(0)
    }

    def validDeclaration(node: IdentNode): Boolean = {
        return symbolTable.lookUp(node.symbolTableName) match {
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
                val res = symbolTable.lookUp(id.symbolTableName)
                res match {
                    case Some(identif) => {
                        identif match {
                            case VarIdentifier(ty) => ty
                            case FuncIdntifier(paramtype, returntype) => returntype
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

    def typeCheck(ty: TypeNode, rhs: RValueNode): Boolean = {
        /* find type of RHS */
        val rhsType = findType(rhs)
        val res = ty match {
            /* basic types (int, bool, char, string) */
            case BaseTypeNode(basicLhs) => basicLhs == rhsType
            /* array type */
            case a: ArrayTypeNode => (a.baseType.typeVal + ":" + a.dimension) == rhsType
            /* pair type */
            case _ => false
        }
        if (!res) {
            errorMessage += "LHS type \"" + ty.typeVal + "\" does not match RHS type \"" + rhsType + "\""
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
                            case ArrayIdentifier(ty,_, _, _)
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