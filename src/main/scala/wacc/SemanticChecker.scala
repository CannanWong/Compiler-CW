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

    /* PAIR REPRESENTATION TBC*/
    def findTypeL(lhs: LValueNode) : String = {
        lhs match {
            case id: IdentNode => {
                val res = symbolTable.lookUpVar(id.name)
                res match {
                    case Some(VarIdentifier(ty)) => ty
                    case Some(ArrayIdentifier(ty, dim, size, elements)) => ty + ":" + dim
                    case Some(PairIdentifier(ty1, ty2)) => ty1 + "-" + ty2
                    // case Some(FuncIdentifier(_, retType)) => {
                    //     errorMessage += "Type error (LHS assignment): cannot assign values to function"    
                    //     "ERROR"
                    // }
                    case None => "ERROR"
                }
            }
            case a: ArrayElemNode => {
                findTypeL(a.ident)
            }
            case f: FstNode => findTypeL(f.lvalue)   // !
            case s: SndNode => findTypeL(s.lvalue)   // !
            case _ => "ERROR" // !
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
                    case Some(ident) => {
                        ident match {
                            case VarIdentifier(ty) => ty
                            // case FuncIdentifier(_, returntype) => returntype
                            case ArrayIdentifier(ty, dim, _, _) => ty + ":" + dim
                            case PairIdentifier(ty1, ty2) => ty1 + "-" + ty2
                        }
                    }
                    case None => {
                        errorMessage += "Typecheck: variable name \"" + id.name + "\" is is not defined\n"
                        "ERROR"
                    }
                }
            }
            case a: ArrayElemNode => {
                val res = symbolTable.lookUpVar(a.ident.name)
                res match {
                    case Some(ident) => {
                        ident match {
                            case ArrayIdentifier(ty,_,_,_) => ty
                            case _ => "ERROR"
                        }
                    }
                    case _ => "ERROR"
                    }
            }
            case u: UnOpExprNode => findTypeR(u.expr)
            case b: BinOpExprNode => findTypeR(b.fstExpr)
            case br: BracketExprNode => findTypeR(br.expr)
            case al: ArrayLiterNode => {
                val fstTy = al.exprList.head
                findTypeR(fstTy)
                // ! Check nested arrays
                // if (findTypeR(fstTy).contains(":")) {
                //     findTypeR(fstTy) + "+1"
                // }
                // else {
                //     findTypeR(fstTy) + ":1"
                // }
            }
            case np: NewPairNode => {
                findTypeR(np.fstExpr) + "-" + findTypeR(np.sndExpr)
            }
            case f: FstNode => findTypeL(f.lvalue)   // !
            case sn: SndNode => findTypeL(sn.lvalue)   // !
            case ca: CallNode => {
                val res = symbolTable.lookUpFunc(ca.ident.name)
                res match {
                    case Some(ident) => {
                        ident match {
                            case FuncIdentifier(_,ty) => ty
                            case _ => "ERROR"
                        }
                    }
                    case _ => "ERROR"
                    }
            }
            case _ => "ERROR"
        }
    }

    def typeCheck(lhs: TypeNode, rhs: RValueNode): Boolean = {
        /* find type of RHS */
        val lhsType = lhs.typeVal
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
}