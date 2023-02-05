package wacc

import scala.collection.mutable

class SymbolTable {

    val map: mutable.Map[String, Identifier] = mutable.Map()

    var size = 0;
    
    def add(name: String, ident: Identifier): Unit = {
        map.addOne(name, ident)
    }

    def lookUp(name: String): Option[Identifier] = {
        return map.get(name)
    }
}

/*
    all variables being globally unique

    renaming:
    <scope index> + ! + variable name
    scope index tracked using a stack
    new scope index assignment = max + 1
*/

