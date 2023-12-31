package wacc

import scala.collection.mutable

import wacc.{FuncIdentifier, ArrayIdentifier, PairIdentifier}
class SymbolTable {

    val map: mutable.Map[String, TypeIdentifier] = mutable.Map()
    var nextFuncNameNum = 1

   
    // Add variable to symbol table
    def addVar(name: String, ty: TypeIdentifier): Unit = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        map.addOne(varName, ty)
    }

    // Add array to symbol table
    def addArray(name: String, ty: TypeIdentifier, dim: Int): Unit = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        val identifier = new ArrayIdentifier(ty, dim)
        map.addOne(varName, identifier)
    }

    // Add pair to symbol table
    def addPair(name: String, ty1: TypeIdentifier, ty2: TypeIdentifier): Unit = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        val identifier = new PairIdentifier(ty1, ty2)
        map.addOne(varName, identifier)
    }
    // Add function to symbol table
    def addFunc(oldName: String, paramtype: List[TypeIdentifier], returntype: TypeIdentifier): String = {
        // Rename function
        val newName = "f" + nextFuncNameNum.toString + "_" + oldName
        val identifier = FuncIdentifier(oldName, paramtype, returntype)
        map.addOne(newName, identifier)
        nextFuncNameNum += 1
        newName
    }

    // Look up variable or array from symbol table in same or higher scopes
    def lookUpVar(name: String): Option[TypeIdentifier] = {
        var idx = 0
        var scope = -1
        var varName = ""
        var identifier: Option[TypeIdentifier] = None
        while (!SemanticChecker.scopeStack.isEmpty && idx < SemanticChecker.scopeStack.size && identifier.isEmpty) {
            scope = SemanticChecker.scopeStack.apply(idx)
            varName = scope.toString() + "!" + name
            identifier = map.get(varName)
            idx += 1
        }
        identifier
    }

    // Look up variable or array from symbol table in same or higher scopes
    def lookUpVarNewName(varName: String): Option[TypeIdentifier] = {
        map.get(varName)
    }

    // Get variable name stored in symbol table
    def getVarName(name: String): String = {
        var idx = 0
        var scope = -1
        var varName = ""
        var identifier: Option[TypeIdentifier] = None
        while (!SemanticChecker.scopeStack.isEmpty && idx < SemanticChecker.scopeStack.size && identifier.isEmpty) {
            scope = SemanticChecker.scopeStack.apply(idx)
            varName = scope.toString() + "!" + name
            identifier = map.get(varName)
            idx += 1
        }
        varName
    }

    // Check if variable is defined in the same scope
    def checkVarDefined(name: String): Boolean = {
        val varName = SemanticChecker.currScope().toString() + "!" + name
        map.contains(varName)
    }

    // Look up function from symbol table
    def lookUpFunc(newName: String): Option[TypeIdentifier] = {
        map.get(newName)
    }

    def getFuncNewName(oldName: String, paramtype: List[TypeIdentifier], returntype: TypeIdentifier): Option[String] = {
        var newName: Option[String] = None
        var funcNameNum = 1
        while (funcNameNum < nextFuncNameNum && newName.isEmpty) {
            val funcName: String = "f" + funcNameNum.toString + "_" + oldName
            // Find renamed function
            map.get(funcName) match {
                case Some(FuncIdentifier(_, plist, retType)) => {
                    val paramstypeValid = (paramtype.length == plist.length) &&
                        paramtype
                        .zip(plist)
                        .map{case (a: TypeIdentifier, b: TypeIdentifier) => a.typeEquals(b)}
                        .fold(true)((x, y) => x && y)
                    // Parameter and return types all match
                    if (paramstypeValid && returntype.typeEquals(retType)) {
                        newName = Some(funcName)
                    }
                }
                case _ =>
            }
            funcNameNum += 1
        }
        newName
    }


    def replaceType(name: String, ty: TypeIdentifier): Unit = {
        map.put(getVarName(name), ty)
    }
    
    override def toString(): String = {
        var ret = ""
        map.foreachEntry((name:String, ty:TypeIdentifier) =>{
            ret += s"{name: ${name}, type: ${ty}}"
        })
        ret
    }
}