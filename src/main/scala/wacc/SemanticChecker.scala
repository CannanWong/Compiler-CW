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
        scopeStack.push(0)
    }

    def validDeclaration(node: IdentNode): Boolean = {
        return symbolTable.lookUp(node.symbolTableName) match {
            case Some(n) => {
                errorMessage += "variable name \"" + node.name + "\" is already used in the same scope\n"
                return false
            }
            case _ => true
        }
    }

    def tableContainsIdentifier(id :IdentNode): Boolean = {
        if (symbolTable.lookUp(id.symbolTableName) == None) {
            errorMessage += "variable name \"" + id.name + "\" is is not defined in this scope\n"
            return false
        }
        return true
    }

    /* PAIR REPRESENTATION TBC*/
    def findTypeL(lhs: LValueNode) : String = {
        return lhs match {
            case id: IdentNode => {
                val res = symbolTable.lookUp(id.symbolTableName)
                return res match {
                    case Some(VarIdentifier(ty)) => ty
                    case Some(ArrayIdentifier(ty, dim, size, elements)) => ty + ":" + dim
                    case Some(PairIdentifier(ty1, ty2)) => ty1 + "-" + ty2
                    case Some(FuncIdntifier(_, retType)) => {
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
                val res = symbolTable.lookUp(id.symbolTableName)
                res match {
                    case Some(identif) => {
                        identif match {
                            case VarIdentifier(ty) => ty
                            case FuncIdntifier(paramtype, returntype) => returntype
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

    def typeCheck(ty: TypeNode, rhs: RValueNode): Boolean = {
        /* find type of RHS */
        val rhsType = findTypeR(rhs)
        val res = ty match {
            /* basic types (int, bool, char, string) */
            case BaseTypeNode(basicLhs) => basicLhs == rhsType
            /* array type */
            case a: ArrayTypeNode => (a.baseType.typeVal + ":" + a.dimension) == rhsType
            /* pair type */
            case _ => false
        }
        if (!res) {
            errorMessage += "LHS type \"" + ty.typeVal + "\" does not match RHS type \"" + rhsType + "\"\n"
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
        return 0
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