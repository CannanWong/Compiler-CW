package wacc


import scala.collection.mutable.ListBuffer

sealed trait ASTNode {
    def semanticCheck(): Unit = {}
}

case class ProgramNode(funcList: List[FuncNode], stat: StatNode) extends ASTNode {

    override def semanticCheck(): Unit = {
        for (f <- funcList) {
            f.addToSymbolTable()
        }
        for (f <- funcList) {
            f.semanticCheck()
        }
        SemanticChecker.insideFunc = false
        stat.semanticCheck()
    }
}

case class FuncNode(ty: TypeNode, ident: IdentNode, paramList: ParamListNode, 
                    stat: StatNode) extends ASTNode {
    def addToSymbolTable(): Unit = {
        ty.semanticCheck()

        // Check if function name is used
        val funcNameUsed = SemanticChecker.symbolTable.lookUpFunc(ident.name) match {
            case Some(n) => {
                SemanticChecker.errorMessage += s"Function \"${ident.name}\" is defined more than once"
                true
            }
            case _ => false
        }

        val paramtypeList = ListBuffer[Identifier]()
        for (param <- paramList.paramList) {
            paramtypeList += param.ty.typeVal()
        }

        // Add to symbol table
        if (!funcNameUsed) {
            SemanticChecker.symbolTable.addFunc(ident.name, paramtypeList.toList, ty.typeVal())
        }
    }
    override def semanticCheck(): Unit = {
        
        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1

        paramList.semanticCheck()

        for (param <- paramList.paramList) {
            // Check for repeated parameter names
            if (SemanticChecker.symbolTable.checkVarDefined(param.ident.name)) {
                SemanticChecker.errorMessage += s"Repeated parameter name \"${param.ident.name}\""
            }
            else {
                SemanticChecker.symbolTable.addVar(param.ident.name, param.ty.typeVal())
            }
        }
        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1

        stat.semanticCheck()
        // Check that return type matches function return type
        checkReturnType(ty, stat)
        SemanticChecker.scopeStack.pop()
        SemanticChecker.scopeStack.pop()
    }

    def checkReturnType(ty: TypeNode, stat: StatNode): Unit = {
        stat match {
            case r: ReturnNode => {
                if (!SemanticChecker.typeCheck(ty.typeVal(), r.expr.typeVal())) {
                    SemanticChecker.errorMessage += "return type should be" +ty.typeVal() + ", unexpected " + r.expr.typeVal() + "\n"
                }
            }
            case b: BeginEndNode => checkReturnType(ty, b.stat)
            case i: IfNode => {
                checkReturnType(ty, i.fstStat)
                checkReturnType(ty, i.sndStat)
            }
            case w: WhileNode => checkReturnType(ty, w.stat)
            case s: StatJoinNode => {
                for (st <- s.statList) {
                    checkReturnType(ty, st)
                }
            }
            case _ =>
        }
    }
}


case class ParamListNode(paramList: List[ParamNode]) extends ASTNode {
    override def semanticCheck(): Unit = {
        for (p <- paramList) {
            p.semanticCheck()
        }
    }
}

case class ParamNode(ty: TypeNode, ident: IdentNode) extends ASTNode {
    override def semanticCheck(): Unit = {
        ty.semanticCheck()
    }
}

// StatNode
sealed trait StatNode extends ASTNode

case class SkipNode() extends StatNode



case class AssignIdentNode(ty: TypeNode, ident: IdentNode, rvalue: RValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        SemanticChecker.debugMessage += s"assignment type of ${ident.name} is ${ty.typeVal().toString()}\n"
        rvalue.semanticCheck()
        
        // Check if variable name is already declared in the same scope
        if (SemanticChecker.symbolTable.checkVarDefined(ident.name)){
             SemanticChecker.errorMessage += s"Variable name \"${ident.name}\" is already used in the same scope"
        }
        // Add to symbol table and get type
         else {
            ty match {
                case b: BaseTypeNode => 
                    SemanticChecker.symbolTable.addVar(ident.name, b.typeVal())
                case a: ArrayTypeNode =>
                    SemanticChecker.symbolTable.addArray(ident.name, a.arrayType, a.arrayDim)
                case p: PairTypeNode =>
                    SemanticChecker.symbolTable.addPair(ident.name, p.fstPET.typeVal(), p.sndPET.typeVal())
            }
            SemanticChecker.debugMessage += "@@@@@@@@ SYMBOL TABLE:@@@@@@@@\n" + SemanticChecker.symbolTable.toString() + "\n"
        }

        if (!SemanticChecker.typeCheck(ty.typeVal(), rvalue.typeVal())) {
            SemanticChecker.errorMessage += "definition: type mismatch: expected "+ ty.typeVal().toString() + " gets " + rvalue.typeVal().toString()
        }
    }        
}


// Example: a=5
case class LValuesAssignNode(lvalue: LValueNode, rvalue: RValueNode) extends StatNode {
    override def semanticCheck(): Unit = { 
        lvalue.semanticCheck()
        rvalue.semanticCheck()
        lvalue match {
            case IdentNode(id) => {
                val func = SemanticChecker.symbolTable.lookUpFunc(id)
                func match {
                    case Some(funcIdent) => SemanticChecker.errorMessage += s"function ${id} cannot be assigned with any values\n"
                    case _ => 
                }
            }
            case _ => 
        }

        rvalue match {
            case IdentNode(id) => {
                val func = SemanticChecker.symbolTable.lookUpFunc(id)
                func match {
                    case Some(funcIdent) =>  SemanticChecker.errorMessage += s"function ${id} cannot be assigned with any values\n"
                    case _ =>
                }
            }
            case _ => 
        }
        
        val lhsType = lvalue.typeVal()
        val rhsType = rvalue.typeVal()
        if (!SemanticChecker.typeCheck(lhsType, rhsType)) {
                SemanticChecker.errorMessage += s"assignment: type mismatch: expected ${lvalue.typeVal().toString}, gets ${rvalue.typeVal().toString()}"
        }

        val undefinedTypeAssign = lhsType match {
            case a: AnyIdentifier => {
                rhsType match {
                    case a: AnyIdentifier => true
                    case _ => false
                }
            }
            case _ => false
        }

        if (undefinedTypeAssign) {
            SemanticChecker.errorMessage += "unidentified type for both LHS RHS type assignment\n"
        }
        
    }

}

case class ReadNode(lvalue: LValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        val ty = lvalue.typeVal()
        SemanticChecker.debugMessage += s"reading type ${ty}\n"
        if ((!SemanticChecker.typeCheck(new IntIdentifier, ty) 
            && !SemanticChecker.typeCheck(new CharIdentifier, ty))) {
            SemanticChecker.errorMessage += s"sread type error: unexpected ${lvalue.typeVal()} (expected: " +
                            "char and int)\n"

        }
    }
}

case class FreeNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        val ty = expr.typeVal()
        // Check if type is array or pair
        if ((!SemanticChecker.typeIsPair(ty) && !SemanticChecker.typeIsArray(ty))) {
            SemanticChecker.errorMessage += "Wrong type in free\n"
        } 
    }
}

case class ReturnNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        if (!SemanticChecker.insideFunc) {
            SemanticChecker.errorMessage += "No return in main\n"
        }
        expr.semanticCheck()
        expr match {
            case i: IdentNode => SemanticChecker.tableContainsIdentifier(i)
            case _ =>
        }
    }
}

case class ExitNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        if (!SemanticChecker.typeCheck(new IntIdentifier, expr.typeVal())) {
            SemanticChecker.errorMessage += "Wrong type in exit\n"
        }
    }
}

case class PrintNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        expr match {
            case i: IdentNode => SemanticChecker.tableContainsIdentifier(i)
            case _ =>
        }
    }
}

case class PrintlnNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        expr match {
            case i: IdentNode => SemanticChecker.tableContainsIdentifier(i)
            case _ =>
        }
    }
}

case class IfNode(expr: ExprNode, fstStat: StatNode, sndStat: StatNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()

        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        fstStat.semanticCheck()
        SemanticChecker.scopeStack.pop()

        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        sndStat.semanticCheck()
        SemanticChecker.scopeStack.pop()
        
        if (!SemanticChecker.typeCheck(new BoolIdentifier, expr.typeVal())) {
            SemanticChecker.errorMessage += "Semantic error in if statement: wrong type in condition\n"
        }
    }
}

case class WhileNode(expr: ExprNode, stat: StatNode) extends StatNode  {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        stat.semanticCheck()
        SemanticChecker.scopeStack.pop()

        if (!SemanticChecker.typeCheck(new BoolIdentifier, expr.typeVal())) {
            SemanticChecker.errorMessage += "Semantic error in while statement: wrong type in condition\n"
        }
    }
}

case class BeginEndNode(stat: StatNode) extends StatNode  {
    override def semanticCheck(): Unit = {
        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        stat.semanticCheck()
        SemanticChecker.scopeStack.pop()
    }
}

case class StatJoinNode(statList: List[StatNode]) extends StatNode  {
    override def semanticCheck(): Unit = {
        for (s <- statList) {
            s.semanticCheck()
        }
    }
}

// LValueNode
sealed trait LValueNode extends ASTNode {
    def typeVal(): Identifier
}

case class IdentNode(name: String) extends LValueNode with ExprNode { 
    override def typeVal() = {
        val identifier = SemanticChecker.symbolTable.lookUpVar(name)
        identifier match {
            case Some(VarIdentifier(tyVal)) => tyVal
            case Some(ty) => ty
            case None => new AnyIdentifier
        }
    }
    
    override def semanticCheck(): Unit = {
        val identifier = SemanticChecker.symbolTable.lookUpVar(name)
        identifier match {
            case None => SemanticChecker.errorMessage += s"${identifier} variable \"${name}\" at scope ${SemanticChecker.currScope()} not in scope or not defined\n" + SemanticChecker.symbolTable.toString + "\n"
            case _ =>
        }
    }
}

// Example: a[1][b]
case class ArrayElemNode(ident: IdentNode, exprList: List[ExprNode]) extends LValueNode with ExprNode {
    var arrayType: Identifier = new AnyIdentifier
    var arrayDim = 0
    override def typeVal() = {
         val identifier = SemanticChecker.symbolTable.lookUpVar(ident.name)
        identifier match {
            case Some(ArrayIdentifier(baseTy: Identifier, dim: Int)) => {
                if (arrayDim > 0) {
                    new ArrayIdentifier(baseTy, arrayDim)
                } else if (arrayDim == 0) {
                    baseTy
                } else {
                    new AnyIdentifier
                }
            }
            case _ => {
                SemanticChecker.errorMessage += s"non array type cannot be indexed\n"
                new AnyIdentifier
            }
        }
     }

    override def semanticCheck(): Unit = {
        if (SemanticChecker.tableContainsIdentifier(ident)) {
            val identifier = SemanticChecker.symbolTable.lookUpVar(ident.name)
            SemanticChecker.debugMessage += s"${ident.name}: ${identifier.toString()} + \n"
            identifier match {
                case (a@Some(ArrayIdentifier(baseTy: Identifier, dim: Int))) => {
                    arrayType = baseTy
                    arrayDim = dim

                    for (e <- exprList) {
                        SemanticChecker.debugMessage += s"{type and dim: ${baseTy} ${dim}} {rxpType: ${e.typeVal()}}\n"
                        arrayDim -= 1
                        e.semanticCheck()
                        if (!e.typeVal().typeEquals(new IntIdentifier)) {
                            SemanticChecker.errorMessage += s"array elem index: unexpected type ${e.typeVal().toString()}, expected int\n"
                        } else {

                        }
                    }
                    if (arrayDim < 0) {
                        SemanticChecker.errorMessage += 
                            s"array type error: unexpected type ${(new ArrayIdentifier(baseTy, exprList.length)).toString()}, expected ${a.value.toString()}\n"
                    }
                }
                case Some(s) => SemanticChecker.debugMessage += s"var ${ident.name} array type, type is ${s.toString()}"
                case None => SemanticChecker.errorMessage += s"variable ${ident.name} \"is not in scope\"\n"
            }
        }
    }
}

sealed trait PairElemNode extends LValueNode with RValueNode

case class FstNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal() = {
        lvalue match {
            case IdentNode(name) => {
                val id = SemanticChecker.symbolTable.lookUpVar(name)
                id match {
                    case Some(ty) => {
                        ty match {
                            case p: PairIdentifier => {
                                SemanticChecker.debugMessage += s"${name} p: type of ty1 is ${p.ty1.toString()}\n"
                                p.ty1
                            }
                            case VarIdentifier(PairIdentifier(ty1, ty2)) => {
                                SemanticChecker.debugMessage += s"${name} v: type of ty1 is ${ty1.toString()}\n"
                                ty1
                            }
                            case _ => new AnyIdentifier
                        }
                    }
                    case _ => new AnyIdentifier
                }
            }
            case _ => new AnyIdentifier
        } 
    }
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
    }
}

case class SndNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal() = {
        lvalue match {
            case IdentNode(name) => {
                val id = SemanticChecker.symbolTable.lookUpVar(name)
                id match {
                    case Some(ty) => {
                        ty match {
                            case p: PairIdentifier => {
                                SemanticChecker.debugMessage += s"${name} p: type of ty2 is ${p.ty2.toString()}\n"
                                p.ty2
                            }
                            case VarIdentifier(PairIdentifier(ty1, ty2)) => {
                                SemanticChecker.debugMessage += s"${name} v: type of ty2 is ${ty1.toString()}\n"
                                ty2
                            }
                            case _ => {
                                SemanticChecker.errorMessage += s"Wrong type assigned for snd, gets ${ty.toString()}\n"
                                new AnyIdentifier
                            }
                        }
                    }
                    case _ => new AnyIdentifier
                }
            }
            case _ => new AnyIdentifier
        } 
    }

    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
    }
}

// RValueNode
sealed trait RValueNode extends ASTNode {
    def typeVal(): Identifier
}

sealed trait ExprNode extends RValueNode

// Example: [1,a] (a=2) / [a,b] (a=[1,2],b=[3,4])
case class ArrayLiterNode(exprList: List[ExprNode]) extends RValueNode {
    override def typeVal(): Identifier = {
        if (!exprList.isEmpty) {
            val elemType = exprList(0).typeVal()
            elemType match {
                case ArrayIdentifier(base, dim) => new ArrayIdentifier(base, dim + 1)
                case t => new ArrayIdentifier(t, 1)
            }
        } else {
            new AnyIdentifier
        }
    }

    override def semanticCheck(): Unit = {
        if (!exprList.isEmpty) {
            val exprTypes:List[Identifier] = exprList.map(expr => expr.typeVal())
            // check whether array literal expr all have same types
            exprTypes
            .zip(exprTypes.map(ty => ty.typeEquals(exprTypes(0))))
            .map{case (x:Identifier, y:Boolean) => if (!y) {
                SemanticChecker.errorMessage += s"array literal expr should have type ${exprTypes(0).toString()}, but was ${x}\n"
            }}
        }
    }
}

// Example: newpair(1,a)
case class NewPairNode(fstExpr: ExprNode, sndExpr: ExprNode) extends RValueNode {
    override def typeVal() = {
        new PairIdentifier(fstExpr.typeVal(), sndExpr.typeVal())
    }
    override def semanticCheck(): Unit = {
        fstExpr.semanticCheck()
        sndExpr.semanticCheck()
    }
}


case class CallNode(ident: IdentNode, argList: ArgListNode) extends RValueNode {
    override def typeVal() = {
        SemanticChecker.symbolTable.lookUpFunc(ident.name) match {
            case Some(FuncIdentifier(_,returntype)) => returntype
            case _ => new AnyIdentifier
        }
    }

    override def semanticCheck(): Unit = {
        argList.semanticCheck()

        SemanticChecker.symbolTable.lookUpFunc(ident.name) match {
            // Check if arg list is of correct type and number
            case Some(FuncIdentifier(paramtype,_)) => {
                if (argList.exprList.length != paramtype.length) {
                        SemanticChecker.errorMessage +=
                            "function argument number is different from function parameter number"
                    }
                else {
                    // Check for tyoe mismatch in function call 
                    val argBuilder = new StringBuilder()
                    val provBuilder = new StringBuilder()
                    val typeCompList = argList.exprList
                                        .map(x => {
                                            val ty = x.typeVal()
                                            // argBuilder builds paramList for error message
                                            if (argBuilder.isEmpty) {argBuilder.++=(s"${ty}")}
                                            else {argBuilder.++=(s", ${ty}")}
                                            // type string gets returned to the list
                                            ty                                           
                                    }).zip(
                                        paramtype
                                        .map(x => {
                                        // provBuilder builds paramList for error message
                                        if (provBuilder.isEmpty) {provBuilder.++=(s"${x}")}
                                        else {provBuilder.++=(s", ${x}")}
                                        // type string gets returned to the list
                                        x 
                                    }))
                    val typeValid = typeCompList
                            .map{case(func: Identifier, prov: Identifier) => func.typeEquals(prov)}
                            .fold(true)((x, y) => x && y)
                            
                    if (!typeValid) {
                        SemanticChecker.errorMessage +=
                            s"function arguement type error: expected: (${argBuilder.toString()})" +
                            s"provided: (${provBuilder.toString()})"
                    }
                }
            }
            case _ =>  SemanticChecker.errorMessage +=
                        "Function name \"" + ident.name + "\" is not defined"
        }
    }
}


case class ArgListNode(exprList: List[ExprNode]) extends ASTNode {
    override def semanticCheck(): Unit = {
        for (e <- exprList) {
            e.semanticCheck()
        }
    }
}

sealed trait TypeNode extends ASTNode {
    def typeVal(): Identifier = new AnyIdentifier
}

case class BaseTypeNode(ty: String) extends TypeNode with PairElemTypeNode {
    // includes: int, char, string, bool
    override def typeVal() = {
        ty match {
            case "int" => new IntIdentifier
            case "bool" => new BoolIdentifier
            case "char" => new CharIdentifier
            case "string" => new StrIdentifier
            case _ => new AnyIdentifier
       }
    }
}


case class ArrayTypeNode(ty: TypeNode) extends TypeNode with PairElemTypeNode {
    var arrayType: Identifier = new AnyIdentifier
    var arrayDim = 1

    override def typeVal(): Identifier = {
        return new ArrayIdentifier(getType(ty).typeVal(), countDimension(ty))
    }

    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        arrayType = getType(ty).typeVal()
        arrayDim = countDimension(ty)
    }

    def countDimension(ty: TypeNode): Int = {
        ty match {
            case ArrayTypeNode(arrayTy) => 1 + countDimension(arrayTy)
            case node => 1
        }  
    }

    // gets the base type of array. ie base type of pair(int, int)[][][] is pair(int, int)
    def getType(ty: TypeNode): TypeNode = {
        ty match {
            case ArrayTypeNode(arrayTy) => getType(arrayTy)
            case node => node
        }  
    }
}

// Example: Pair(bool, int[])
case class PairTypeNode(fstPET: PairElemTypeNode, sndPET: PairElemTypeNode) extends TypeNode {
    override def typeVal(): Identifier = {
        new PairIdentifier(fstPET.typeVal(), sndPET.typeVal())
    }

    override def semanticCheck(): Unit = {
        fstPET.semanticCheck()
        sndPET.semanticCheck()
    }
}

// PairElemTypeNode
sealed trait PairElemTypeNode extends ASTNode {
    def typeVal(): Identifier = new PairIdentifier(new AnyIdentifier, new AnyIdentifier)
}
// <pair>
case class PETPairNode() extends PairElemTypeNode {
    override def typeVal(): Identifier = super[PairElemTypeNode].typeVal()
}

case class IntLiterNode(n: Int) extends ExprNode {
    override def typeVal() = new IntIdentifier
}

case class BoolLiterNode(b: Boolean) extends ExprNode {
    override def typeVal() = new BoolIdentifier
}

case class CharLiterNode(c: Char) extends ExprNode {
    override def typeVal() = new CharIdentifier
}

case class StrLiterNode(s: String) extends ExprNode {
    override def typeVal() = new StrIdentifier
    // override def semanticCheck(): Unit = {
    //     if (s.contains("\n")) {
    //         SemanticChecker.errorMessage += "String cannot contain newline\n"
    //     }
    // }
}

case class PairLiterNode() extends ExprNode {
    // val: null
    override def typeVal() = new NullIdentifier
}

sealed trait UnOpExprNode extends ExprNode

// bool -> bool
case class NotNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = new BoolIdentifier
    override def semanticCheck(): Unit = {
        if(!SemanticChecker.typeCheck(new BoolIdentifier, expr.typeVal())) {
            SemanticChecker.errorMessage += s"Unary op: expression unexpected type ${expr.typeVal().toString()}, expected type bool\n"
        }
    }
}

// int -> int
case class NegNode(expr: ExprNode) extends UnOpExprNode {
     override def typeVal() = new IntIdentifier
    override def semanticCheck(): Unit = {
        if(!SemanticChecker.typeCheck(new IntIdentifier, expr.typeVal())) {
            SemanticChecker.errorMessage += s"Unary op: expression unexpected type ${expr.typeVal().toString()}, expected type int\n"
        }
    }
}

// array -> int
case class LenNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = new IntIdentifier
    override def semanticCheck(): Unit = {
        expr.typeVal() match {
            case a: ArrayIdentifier => 
            case _ => SemanticChecker.errorMessage += "Type error: expected at least 1-dimensional array.\n"
        }
    }
}

// char -> int
case class OrdNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = new IntIdentifier
    override def semanticCheck(): Unit = {
        if(!SemanticChecker.typeCheck(new CharIdentifier, expr.typeVal())) {
            SemanticChecker.errorMessage += s"Unary op: expression unexpected type ${expr.typeVal().toString()}, expected type char\n"
        }
    }
}

// int -> char
case class ChrNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = new CharIdentifier
    override def semanticCheck(): Unit = {
        if(!SemanticChecker.typeCheck(new IntIdentifier, expr.typeVal())) {
            SemanticChecker.errorMessage += s"Unary op: expression unexpected type ${expr.typeVal().toString()}, expected type int\n"
        }
    }
}

sealed trait BinOpExprNode extends ExprNode {
    override def typeVal(): Identifier = {
        this match {
            case MulNode(_,_) | DivNode(_,_) | ModNode(_,_) | AddNode(_,_) | SubNode(_,_) => new IntIdentifier
            case _ => new BoolIdentifier
            }
    }
    override def semanticCheck(): Unit = {
        this match {
            case MulNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int\n"
                    }
            }
            case DivNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int\n"
                    }
            }
            case ModNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int\n"
                    }
            }
            case AddNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int\n"
                    }
            }
            case SubNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int\n"
                    }
            }
            case GTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal()) && !SemanticChecker.typeCheck(new CharIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int/char\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal()) && !SemanticChecker.typeCheck(new CharIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int/char\n"
                    }
            }
            case GTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal()) && !SemanticChecker.typeCheck(new CharIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int/char\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal()) && !SemanticChecker.typeCheck(new CharIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int/char\n"
                    }
            }
            case LTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal()) && !SemanticChecker.typeCheck(new CharIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int/char\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal()) && !SemanticChecker.typeCheck(new CharIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int/char\n"
                    }
            }
            case LTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(new IntIdentifier, fstExpr.typeVal()) && !SemanticChecker.typeCheck(new CharIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type int/char\n"
                    }
                    if (! SemanticChecker.typeCheck(new IntIdentifier, sndExpr.typeVal()) && !SemanticChecker.typeCheck(new CharIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type int/char\n"
                    }
            }
            case EqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(fstExpr.typeVal(), sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary eq op: unexpected lhs type ${fstExpr.typeVal().toString()}, expected rhs type ${sndExpr.typeVal().toString()}\n"
                    }
            }
            case IEqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(fstExpr.typeVal(), sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary eq op: unexpected lhs type ${fstExpr.typeVal().toString()}, expected rhs type ${sndExpr.typeVal().toString()}\n"
                    }
            }
            case AndNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(new BoolIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type bool\n"
                    }
                    if (!SemanticChecker.typeCheck(new BoolIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type bool\n"
                    }
            }
            case OrNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(new BoolIdentifier, fstExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal().toString()}, expected lhs type bool\n"
                    }
                    if (! SemanticChecker.typeCheck(new BoolIdentifier, sndExpr.typeVal())) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${sndExpr.typeVal().toString()}, expected lhs type bool\n"
                    }
            }
        }
    }
}

case class MulNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class DivNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class ModNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class AddNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class SubNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class GTNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class GTENode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class LTNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class LTENode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class EqNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class IEqNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class AndNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode
case class OrNode(fstexpr: ExprNode, sndexpr: ExprNode) extends BinOpExprNode

case class UnaryOperNode(op: String) extends ASTNode
case class BinaryOperatorNode(op: String) extends ASTNode 
