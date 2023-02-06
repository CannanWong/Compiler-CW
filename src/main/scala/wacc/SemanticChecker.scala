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

    def validDeclaration(id: IdentNode): Boolean = {
        symbolTable.lookUpVar(id.name) match {
            case Some(n) => {
                errorMessage += "variable name \"" + id.name + "\" is already used in the same scope\n"
                return false
            }
            case _ => true
        }
    }

    def tableContainsIdentifier(id :IdentNode): Boolean = {
        if (symbolTable.lookUpVar(id.name) == None) {
            errorMessage += "variable name \"" + id.name + "\" is is not defined in this scope\n"
            return false
        }
        return true
    }

    /* function returns -1 if idNode is not definded in any scope */
    def identifierScope(id: IdentNode): Int = {
        var ret = -1
        /* can only return 0 or 1 result */
        var idx = 0
        while (idx < scopeStack.size && ret != -1) {
            val scope = scopeStack.indexOf(idx)
            symbolTable.lookUp(s"${scope}!" + id.name) match {
                case Some(identif) => {
                    ret = scope
                }
                case None => 
            }
            idx += 1
        }
        ret
    }

    /* PAIR REPRESENTATION TBC*/
    def findTypeL(lhs: LValueNode) : String = {
        lhs match {
            case id: IdentNode => {
                val res = symbolTable.lookUpVar(id.name)
                res match {
                    case Some(VarIdentifier(ty)) => ty
                    case Some(ArrayIdentifier(ty, dim, size, elements)) => ty + ":" + dim
                    case Some(PairIdentifier(ty1, ty2)) => ty1 + "-" + ty2
                    case Some(FuncIdentifier(_, retType)) => {
                        errorMessage += "Type error (LHS assignment): cannot assign values to function"    
                        "ERROR"
                    }
                        
                    case None => "ERROR"
                }
            }
            case ArrayElemNode(id, _) => {
                findTypeL(id)
            }
            case FstNode(lvalue) => ???
            case SndNode(lvalue) => ???
            case _ => ???
        }
    }

    /* PAIR REPRESENTATION TBC*/
    def findTypeR(rhs: RValueNode) : String = {
        return rhs match {
            /* basic type literals (int, bool, char, string) */
            case i: IntLiterNode => "int"
            case b: BoolLiterNode => "bool"
            case c: CharLiterNode => "char"
            case s: StrLiterNode => "str"
            /* identifier (var and func) */
            case id: IdentNode => {
                val res = symbolTable.lookUpVar(id.name)
                res match {
                    case Some(identif) => {
                        identif match {
                            case VarIdentifier(ty) => ty
                            case FuncIdentifier(paramtype, returntype) => returntype
                            case ArrayIdentifier(ty, dim, size, elements) => ty + ":" + dim
                            case PairIdentifier(ty1, ty2) => ty1 + "-" + ty2
                        }
                    }
                    case None => {
                        errorMessage += "Typecheck: variable name \"" + id.name + "\" is is not defined\n"
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
        val rhsType = findTypeR(rhs)
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
        val res = findTypeL(lhs) == findTypeR(rhs)
        if (!res) {
            errorMessage += "LHS type \"" + findTypeL(lhs) + "\" does not match RHS type \"" + findTypeR(rhs) + "\"\n"
        }
        return res
    }

    def currScope(): Int = {
        return scopeStack.top
    }

    def checkIfWhileCond(expr: ExprNode): Unit = {
        expr match {
            case IntLiterNode(_) | CharLiterNode(_) | StrLiterNode(_) | PairLiterNode()
                => errorMessage += "Semantic error in if statement: wrong type in condition\n"
            case id: IdentNode
                // Check symbol table if variable is of type bool
                => {
                    if (tableContainsIdentifier(id)) {
                        val identifier = symbolTable.lookUpVar(id.name).get
                        identifier match {
                            case VarIdentifier(ty)
                                => if (ty != "bool") {
                                    errorMessage += "Semantic error in if statement: wrong type in condition\n"
                                }
                            case _
                               => errorMessage += "[Not possible!] Semantic error in if statement\n" 
                        }
                    }
                }
            case ArrayElemNode(id, exprList)
                // Check symbol table if array elem is of type bool
                => {
                    if (tableContainsIdentifier(id)) {
                        val identifier = symbolTable.lookUpVar(id.name).get
                        identifier match {
                            case ArrayIdentifier(ty,_, _, _)
                                => if (exprList.length == 1) {
                                    if (ty != "bool") {
                                        errorMessage += "Semantic error in if statement: wrong type in condition\n"
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
                        errorMessage += "Semantic error in if statement: wrong type in condition\n"
                    }
            case BinOpExprNode(_,BinaryOperatorNode(op),_)
                => if (op == "*" || op == "/" || op == "%" || op == "+" || op == "-") {
                        errorMessage += "Semantic error in if statement: wrong type in condition\n"
                    }
            case _ =>
        }
    }
}