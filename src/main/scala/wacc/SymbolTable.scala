package wacc

import scala.collection.mutable

class SymbolTable {

    val map: mutable.Map[String, Identifier] = mutable.Map()

    var size = 0; // ?
    
    // Add variable to symbol table
    def addVar(name: String, ty: String): Unit = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        val identifier = new VarIdentifier(ty)
        map.addOne(varName, identifier)
    }

    // Add array to symbol table
    def addArray(name: String, ty: String, dim: Int): Unit = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        val identifier = new ArrayIdentifier(ty, dim)
        map.addOne(varName, identifier)
    }

    // Add pair to symbol table
    def addPair(name: String, ty1: String, ty2: String): Unit = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        val identifier = new PairIdentifier(ty1, ty2)
        map.addOne(varName, identifier)
    }

    // Add function to symbol table
    def addFunc(name: String, paramtype: List[String], returntype: String): Unit = {
        val funcName = "f!" + name
        val identifier = new FuncIdentifier(paramtype, returntype)
        map.addOne(funcName, identifier)
    }

    // Look up variable or array from symbol table in same or higher scopes
    def lookUpVar(name: String): Option[Identifier] = {
        var idx = 0
        var scope = SemanticChecker.scopeStack.indexOf(idx)
        var varName = scope.toString() + "!" + name
        var identifier = map.get(varName)
        while (idx < SemanticChecker.scopeStack.size && identifier.isEmpty) {
            idx += 1
            scope = SemanticChecker.scopeStack.indexOf(idx)
            varName = scope.toString() + "!" + name
            identifier = map.get(varName)
        }
        identifier
    }

    // Check if variable is defined in the same scope
    def checkVarDefined(name: String): Boolean = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        map.contains(varName)
    }

    // Look up function from symbol table
    def lookUpFunc(name: String): Option[Identifier] = {
        val funcName = "f!" + name
        map.get(funcName)
    }
}

/*
    all variables being globally unique

    renaming:
    <scope index> + ! + variable name
    scope index tracked using a stack
    new scope index assignment = max + 1
*/

