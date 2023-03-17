package wacc

import scala.collection.mutable.Stack

import wacc.{ArrayIdentifier, PairIdentifier, NullIdentifier, AnyIdentifier}
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

    def tableContainsIdentifier(id :IdentNode): Boolean = {
        symbolTable.lookUpVar(id.name) match {
            case None => {
                Error.addSemErr("variable name \"" + id.name + s"\" is is not defined in this scope")
                false
            }
            case _ => true
        }
    }

    // for AssignIdentNode
    def typeCheck(ty1: TypeIdentifier, ty2: TypeIdentifier): Boolean = {
        ty1 match {
            case p1: PairIdentifier => {
                ty2 match {
                    case p2: PairIdentifier => {
                        typeCheckPair(p1, p2)
                    }
                    case _ => {
                        ty2.typeEquals(p1)
                    }
                }        
            }
            case _ => {
                ty1.typeEquals(ty2)}
        }
    }


    def typeCheckPair(lhsType: PairIdentifier, rhsType: PairIdentifier) : Boolean = {
        var ret = true
        val lhsFstType = lhsType.ty1
        val rhsFstType = rhsType.ty1
        if (!lhsFstType.typeEquals(rhsFstType)) {
            Error.addSemErr("Wrong type in pair type, expected fst type to be " +
                            lhsFstType + " instead of " + rhsFstType)
            ret = false
        }
        
        val lhsSndType = lhsType.ty2
        val rhsSndType = rhsType.ty2
        if (!lhsSndType.typeEquals(rhsSndType)) {
            Error.addSemErr("Wrong type in pair type, expected snd type to be " +
                            lhsSndType + " instead of " + rhsSndType)
            ret = false
        }        
        ret    
    }


    def typeIsArray(id: TypeIdentifier): Boolean = {
        id match {
        case ar: ArrayIdentifier=> true
        case a: AnyIdentifier => true
        case _ => false
        }
    }

    def typeIsPair(id: TypeIdentifier): Boolean = {
        id match {
        case p: PairIdentifier => true
        case n: NullIdentifier => true
        case a: AnyIdentifier => true
        case _ => false
        }
    }

    def currScope(): Int = {
        return scopeStack.top
    }

    def getNewType(lhsTy: TypeIdentifier, rhsTy: TypeIdentifier): TypeIdentifier = {
        if (!typeCheck(lhsTy, rhsTy)) {
            throw new IllegalArgumentException(s"lhs ${lhsTy} type is not replacable by rhs type ${rhsTy}")
        } else if (lhsTy.isAbstract()) {
            /*  type of lhs is not complete and can be replaced if a more complete type exists on rhs.
                if lhs typenode idx > rhs typenode index, replace node */
            lhsTy match {
                case n: NullIdentifier 
                    => if (rhsTy.concretenessIndex > n.concretenessIndex) rhsTy else lhsTy
                case PairIdentifier(ty1, ty2) => {
                    rhsTy match {
                        case PairIdentifier(rty1, rty2) => {
                            // replace ty1
                            val newTy1 = 
                                if (ty1.concretenessIndex < rty1.concretenessIndex) {
                                    rty1
                                } else {
                                    getNewType(ty1, rty1)
                                }
                            
                            // replace ty2
                            val newTy2 = 
                                if (ty2.concretenessIndex < rty2.concretenessIndex) {     
                                    rty2
                                } else {
                                    getNewType(ty2, rty2)
                                }
                                
                            val pty = PairIdentifier(newTy1, newTy2)
                            pty
                        }
                        case NullIdentifier() => lhsTy
                        case _ =>  lhsTy // attempts to repace pair with list type
                    }
                }
                case ArrayIdentifier(base, dim) => {
                    rhsTy match {
                        case ArrayIdentifier(rBase, _) => {
                            // replace base
                            val newBaseTy = 
                                if (base.concretenessIndex < rBase.concretenessIndex) {
                                    rBase
                                } else {
                                    getNewType(base, rBase)
                                }
                            ArrayIdentifier(newBaseTy, dim)
                        }
                        case _
                        // attempts to repalce array with non-array type
                            => throw new IllegalArgumentException(s"lhs ${lhsTy} type is not " +
                              s"replacable by rhs type ${rhsTy}")
                    }
                }
                // returns original lhs type if no type replacement occurs
                case t => lhsTy
            } 
        }else {
            // returns original lhs type if no type replacement occurs
            lhsTy
        }      
        
    }

    def replaceType (lvalue: LValueNode, newTy: TypeIdentifier): Unit = {
        lvalue match {
            case IdentNode(name) => symbolTable.replaceType(name, newTy)
            case a: ArrayElemNode => /* semantic error: array type cannot be abstract */
            case FstNode(lval) => replaceType(lval, PairIdentifier( newTy, SndNode(lval).typeVal()))
            case SndNode(lval) => replaceType(lval, PairIdentifier(FstNode(lval).typeVal(), newTy))
        }
    }
}