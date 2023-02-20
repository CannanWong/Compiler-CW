package wacc

import scala.collection.mutable.Stack

object SemanticChecker {
    var debugMessage = ""
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
        debugMessage = ""
        errorMessage = ""
        symbolTable = new SymbolTable()
        scopeStack.push(0)
        nextScope = 1
    }

    def tableContainsIdentifier(id :IdentNode): Boolean = {
        symbolTable.lookUpVar(id.name) match {
            case None => {
                errorMessage += "variable name \"" + id.name + s"\" is is not defined in this scope (scope ${SemanticChecker.currScope()})\n"
                false
            }
            case _ => true
        }
    }

    // for AssignIdentNode
    def typeCheck(ty1: Identifier, ty2: Identifier): Boolean = {
        ty1 match {
            case p1: PairIdentifier =>{ 
                ty2 match {
                    case p2: PairIdentifier => typeCheckPair(p1, p2)
                    case _ => ty2.typeEquals(p1)
                }
                
            }
            case _ => ty1.typeEquals(ty2)
        }
    }


    def typeCheckPair(lhsType: PairIdentifier, rhsType: PairIdentifier) : Boolean = {
        var ret = true
        val lhsFstType = lhsType.ty1
        val rhsFstType = rhsType.ty1
        SemanticChecker.debugMessage += s"fst type 1:${lhsFstType} 2:${rhsFstType}\n"
        if (!lhsFstType.typeEquals(new NullIdentifier) || !rhsFstType.typeEquals(new NullIdentifier) ||
            !lhsFstType.typeEquals(new AnyIdentifier) || !rhsFstType.typeEquals(new AnyIdentifier)) {
            if (!lhsFstType.typeEquals(rhsFstType)) {
                SemanticChecker.errorMessage += "Wrong type in pair declaration, expected " +
                                lhsFstType + " instead of " + rhsFstType +"\n"
                ret = false
            }
        }
        val lhsSndType = lhsType.ty2
        val rhsSndType = rhsType.ty2
        SemanticChecker.debugMessage += s"snd type 1:${lhsSndType} 2:${rhsSndType}\n"
        if (!lhsSndType.typeEquals(new NullIdentifier) || !rhsSndType.typeEquals(new NullIdentifier) ||
            !lhsSndType.typeEquals(new AnyIdentifier) || !rhsSndType.typeEquals(new AnyIdentifier)) {
            if (!lhsSndType.typeEquals(rhsSndType)) {
                SemanticChecker.errorMessage += "Wrong type in pair declaration, expected " +
                                lhsSndType + " instead of " + rhsSndType +"\n"
                ret = false
            }
        }
        // if (lhsFstType.typeEquals(new AnyIdentifier) && rhsFstType.typeEquals(new AnyIdentifier)
        // || lhsSndType.typeEquals(new AnyIdentifier) && rhsSndType.typeEquals(new AnyIdentifier)) {
        //     SemanticChecker.errorMessage += "Undefined type in assignment"
        //     ret = false
        // }
        
        ret    
    }


    def typeIsArray(id: Identifier): Boolean = {
        id match {
        case ar: ArrayIdentifier=> true
        case VarIdentifier(varId) => typeIsArray(varId)
        case a: AnyIdentifier => true
        case _ => false
        }
    }

    def typeIsPair(id: Identifier): Boolean = {
        id match {
        case p: PairIdentifier => true
        case n: NullIdentifier => true
        case VarIdentifier(varId) => typeIsArray(varId)
        case a: AnyIdentifier => true
        case _ => false
        }
    }

    def currScope(): Int = {
        return scopeStack.top
    }
}