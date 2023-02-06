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
    def addArray(name: String, ty: String, dim: Int, size: Int, elements: List[Any]): Unit = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        val identifier = new ArrayIdentifier(ty, dim, size, elements)
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

    // Look up variable or array from symbol table
    def lookUpVar(name: String): Option[Identifier] = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        return map.get(varName)
    }

    // Look up function from symbol table
    def lookUpFunc(name: String): Option[Identifier] = {
        val funcName = "f!" + name
        return map.get(funcName)
    }

    // Look up any from symbol table
    def lookUp(tableName: String): Option[Identifier] = {
        return map.get(tableName)
    }
}

/*
    all variables being globally unique

    renaming:
    <scope index> + ! + variable name
    scope index tracked using a stack
    new scope index assignment = max + 1
*/

