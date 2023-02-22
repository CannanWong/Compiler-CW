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
            case p1: PairIdentifier =>{ 
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
        if (!lhsFstType.typeEquals(new NullIdentifier) || !rhsFstType.typeEquals(new NullIdentifier) ||
            !lhsFstType.typeEquals(new AnyIdentifier) || !rhsFstType.typeEquals(new AnyIdentifier)) {
            if (!lhsFstType.typeEquals(rhsFstType)) {
                Error.addSemErr("Wrong type in pair declaration, expected " +
                                lhsFstType + " instead of " + rhsFstType)
                ret = false
            }
        }
        val lhsSndType = lhsType.ty2
        val rhsSndType = rhsType.ty2
        if (!lhsSndType.typeEquals(new NullIdentifier) || !rhsSndType.typeEquals(new NullIdentifier) ||
            !lhsSndType.typeEquals(new AnyIdentifier) || !rhsSndType.typeEquals(new AnyIdentifier)) {
            if (!lhsSndType.typeEquals(rhsSndType)) {
                Error.addSemErr("Wrong type in pair declaration, expected " +
                                lhsSndType + " instead of " + rhsSndType)
                ret = false
            }
        }        
        ret    
    }


    def typeIsArray(id: TypeIdentifier): Boolean = {
        id match {
        case ar: ArrayIdentifier=> true
        // case VarIdentifier(varId) => typeIsArray(varId)
        case a: AnyIdentifier => true
        case _ => false
        }
    }

    def typeIsPair(id: TypeIdentifier): Boolean = {
        id match {
        case p: PairIdentifier => true
        case n: NullIdentifier => true
        // case VarIdentifier(varId) => typeIsArray(varId)
        case a: AnyIdentifier => true
        case _ => false
        }
    }

    def currScope(): Int = {
        return scopeStack.top
    }
}