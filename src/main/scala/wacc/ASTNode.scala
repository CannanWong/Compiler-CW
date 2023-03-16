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
    override def semanticCheck(): Unit = {
        /* abstract type semantic error: must be a non-replacable type */
        if(ty.typeVal().isRepacable()) {
            Error.addSemErr(s"function ${ident.name} cannot return abstract type ${ty.typeStrVal()}")
        }

        ident.isFunction = true
        
        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1

        paramList.semanticCheck()

        /* abstract type semantic error: function param type unknown for function overloading */
        if (paramList.abstractDef) {
            Error.addSemErr(s"Unknown function type: abstract type definition of " +
              s"function parameter for function \"${ident.name}\"")
        }

        for (param <- paramList.paramList) {
            // Check for repeated parameter names
            if (SemanticChecker.symbolTable.checkVarDefined(param.ident.name)) {
                 Error.addSemErr(s"Repeated parameter name \"${param.ident.name}\"")
            }
            else {
                SemanticChecker.symbolTable.addVar(param.ident.name, param.ty.typeVal())
                param.ident.newName = SemanticChecker.currScope().toString() + "!" + param.ident.name
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
                    Error.addSemErr("return type should be" + ty.typeVal() + 
                                    ", unexpected " + r.expr.typeVal())
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

    def addToSymbolTable(): Unit = {
        ty.semanticCheck()

        val paramtypeList = ListBuffer[TypeIdentifier]()
        for (param <- paramList.paramList) {
            paramtypeList += param.ty.typeVal()
        }

        // Check if function name is used
        SemanticChecker.symbolTable.getFuncNewName(ident.name, paramtypeList.toList, ty.typeVal()) 
        match {
            case Some(newName) => {
                Error.addSemErr(s"Function \"${ident.name}\" is defined more than once " +
                                "with the same parameter and return types")
            }
            // Add to symbol table
            case _ => {
                ident.newName = SemanticChecker.symbolTable.addFunc(ident.name, 
                                paramtypeList.toList, ty.typeVal())
            }
        }
    }
}


case class ParamListNode(paramList: List[ParamNode]) extends ASTNode {
    var abstractDef = false
    override def semanticCheck(): Unit = {
        for (p <- paramList) {
            p.semanticCheck()
            /* abstract type semantic error: function param unknown for function overloading */
            if (p.ty.typeVal().isRepacable()) {
                abstractDef = true
            }
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
        rvalue match {
            case c: CallNode => c.returnType = Some(ty.typeVal())
            case _ =>
        }
        rvalue.semanticCheck()

        val rValTyval = rvalue.typeVal()
        val tyTyval = ty.typeVal()


        // Check if variable name is already declared in the same scope
        if (SemanticChecker.symbolTable.checkVarDefined(ident.name)){
            Error.addSemErr(s"Variable name \"${ident.name}\" is already used in the same scope")
        }
        // Add to symbol table and get type
        else {
            /* abstract type semantic error: function call is assigned to an abstract type when function
           overloading exists */
        rvalue match {
            case CallNode(callident, argList) => {
                if (ty.typeVal().isRepacable()) {
                     Error.addSemErr(s"Function return type unknown, cannot assign result from function " +
                       s"call ${callident.name} to \"${ident.name}\" of abstract type ${ty.typeStrVal()}")
                }
            }
            case _ => {
                // check if abstact type is decalred
                if (!rValTyval.isFullType() && !tyTyval.isFullType()) {
                    Error.addSemErr(
                        s"abstract type error: \"${ident.name}\" is declared as an abstract type " +
                        s"${ty.typeStrVal()} for abstract value with type ${rValTyval}"
                        )
                }
            }
        }

            val storedType = if (tyTyval.isFullType()) tyTyval else rValTyval  
            SemanticChecker.symbolTable.addVar(ident.name, storedType)

            ident.newName = SemanticChecker.currScope().toString() + "!" + ident.name
        }

        /* check whether rvalue type matches that of TypeNode */
        if (!SemanticChecker.typeCheck(tyTyval, rValTyval)) {
            Error.addSemErr("definition: type mismatch: expected "+ tyTyval.toString()
                        + s" for ${ident.name}, gets "
                        + rvalue.typeVal().toString())
        }
    }        
}


// Example: a=5
case class LValuesAssignNode(lvalue: LValueNode, rvalue: RValueNode) extends StatNode {
    override def semanticCheck(): Unit = { 
        lvalue.semanticCheck()
        rvalue match {
            case c: CallNode => c.returnType = Some(lvalue.typeVal())
            case _ =>
        }

        /* abstract type semantic error: function call is assigned to an abstract type when function
           overloading exists */
        rvalue match {
            case CallNode(ident, argList) => {
                if (lvalue.typeVal().isRepacable()) {
                     Error.addSemErr(s"Function return type unknown, cannot " +
                       s"assign to lhs value with an abstract type ${lvalue.typeVal()}")
                }
            }
            case _ =>
        }
        rvalue.semanticCheck()
        
        val lhsType = lvalue.typeVal()
        val rhsType = rvalue.typeVal()
        if (!SemanticChecker.typeCheck(lhsType, rhsType)) {
                Error.addSemErr(s"assignment: type mismatch: expected ${lvalue.typeVal().toString}, " +
                                s"gets ${rvalue.typeVal().toString()}")
        }else {
            //lvalue will update type if type if type is known from assignment
            val newLvalueType = SemanticChecker.getNewType(lhsType, rhsType)
            SemanticChecker.replaceType(lvalue, newLvalueType)
        }
    }
}

case class ReadNode(lvalue: LValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        lvalue.typeVal() match {
            case IntIdentifier() | CharIdentifier() | AnyIdentifier() | NullIdentifier() =>
            case ty => Error.addSemErr(s"read type error: unexpected ${lvalue.typeVal()}" +
                    "(expect variable with type char or int)")
        }
    }
}

case class FreeNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        val ty = expr.typeVal()
        // Check if type is array or pair
        if ((!SemanticChecker.typeIsPair(ty) && !SemanticChecker.typeIsArray(ty))) {
            Error.addSemErr(s"free type error: unexpected ${ty} (expected: at least " +
                            "1-dimension array or pair)")
        } 
    }
}

case class ReturnNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        if (!SemanticChecker.insideFunc) {
            Error.addSemErr("return outside of function is not allowed")
        }
        expr.semanticCheck()
        expr match {
            case i: IdentNode => SemanticChecker.tableContainsIdentifier(i)
            case a: ArrayElemNode => SemanticChecker.tableContainsIdentifier(a.ident)
            case _ =>
        }
    }
}

case class ExitNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        if (!SemanticChecker.typeCheck(IntIdentifier(), expr.typeVal())) {
            Error.addSemErr(s"free type error: unexpected ${expr.typeVal()} (expected: int)")
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
        
        if (!SemanticChecker.typeCheck(BoolIdentifier(), expr.typeVal())) {
            Error.addSemErr(s"if condition type error: unexpected ${expr.typeVal()} " +
                            "(expected: bool)")
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

        if (!SemanticChecker.typeCheck(BoolIdentifier(), expr.typeVal())) {
            Error.addSemErr(s"while condition type error: unexpected ${expr.typeVal()} " +
                            "(expected: bool)")
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
    def typeVal(): TypeIdentifier
}

case class IdentNode(name: String) extends LValueNode with ExprNode {            
    var newName: String = null
    var isFunction: Boolean = false
    // Store type to prevent frequent lookup from symbol table
    var ty: Option[TypeIdentifier] = None

    override def typeVal() = {
        ty match {
            case Some(tyIdent) => tyIdent
            case _ => {
                SemanticChecker.symbolTable.lookUpVar(name) match {
                    case Some(tyIdent) => {
                        ty = Some(tyIdent)
                        tyIdent
                    }
                    case None => {
                        ty = Some(AnyIdentifier())
                        AnyIdentifier()
                    }
                }
            }
        }
    }

    override def semanticCheck(): Unit = {
        val identifier = SemanticChecker.symbolTable.lookUpVar(name)
        identifier match {
            case None => Error.addSemErr(s"variable \"${name}\" is not in scope or not defined")
            case _ => {
                if (!isFunction) {
                    newName = SemanticChecker.symbolTable.getVarName(name)
                }
            }
        }

    }
}

// Example: a[1][b]
case class ArrayElemNode(ident: IdentNode, exprList: List[ExprNode]) extends LValueNode with ExprNode {
    /* temporary type val before typeval is called */
    var arrayType: TypeIdentifier = AnyIdentifier()
    var arrayDim = 0

    override def typeVal() = {
        val identifier = SemanticChecker.symbolTable.lookUpVar(ident.name)
        identifier match {
            case Some(ArrayIdentifier(baseTy: TypeIdentifier, dim: Int)) => {
                arrayDim = dim - exprList.length
                if (arrayDim > 0) {
                    ArrayIdentifier(baseTy, arrayDim)
                } else if (arrayDim == 0) {
                    baseTy
                } else {
                    /* semantic error: expr list longer than dimension of array declared for var */
                    AnyIdentifier()                                    
                }
            }
            /* semantic error: non-array elem should not be assigned as arrayelem node */
            case Some(ty) => AnyIdentifier()
            /* semantic error: variable not defined */
            case None => AnyIdentifier()
        }
    }
    override def semanticCheck(): Unit = {
        if (SemanticChecker.tableContainsIdentifier(ident)) {
            SemanticChecker.symbolTable.lookUpVar(ident.name) match {
                case Some(a@ArrayIdentifier(_,_)) => {
                    arrayType = a.baseTy
                    arrayDim = a.dim
                    for (e <- exprList) {
                        arrayDim -= 1
                        e.semanticCheck()
                        if (!e.typeVal().typeEquals(IntIdentifier())) {
                            Error.addSemErr(s"array elem index: unexpected type " +
                                            s"${e.typeVal().toString()}, expected int")
                        }
                    }
                    if (arrayDim < 0) {
                        Error.addSemErr(s"array dimension error: unexpected type " +
                                        s"${(ArrayIdentifier(a.baseTy, exprList.length)).toString()}" +
                                        s"at ${ident.name}, expected ${a.toString()}")   
                    }
                }
                case None => Error.addSemErr(s"variable \"${ident.name}\" is not in scope")
                case ty => Error.addSemErr(s"expect at least 1 dimension array, gets ${ty.toString()}")
            }
        }
        ident.semanticCheck()
    }
}

sealed trait PairElemNode extends LValueNode with RValueNode

case class FstNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal() = {
        val ty = lvalue.typeVal()

        ty match {
            case p: PairIdentifier => p.ty1
            case n: NullIdentifier => n
            case _ => AnyIdentifier()       /* semantic error: wrong lvalue type*/
        } 
    }
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()

        val ty = lvalue.typeVal()
        ty match {
            case PairIdentifier(_,_) | AnyIdentifier() | NullIdentifier() => 
            case _ => Error.addSemErr(s"Wrong type assigned for fst, gets ${ty.toString()}")
        }
    }
}

case class SndNode(lvalue: LValueNode) extends PairElemNode {
        override def typeVal() = {
        val ty = lvalue.typeVal()

        ty match {
            case p: PairIdentifier => p.ty2
            case n: NullIdentifier => n
            case _ => AnyIdentifier()       /* semantic error: wrong lvalue type*/
        } 
    }
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()

        val ty = lvalue.typeVal()
        ty match {
            case PairIdentifier(_,_) | AnyIdentifier() | NullIdentifier() => 
            case _ => Error.addSemErr(s"Wrong type assigned for snd, gets ${ty.toString()}")
        }
    }
}

// RValueNode
sealed trait RValueNode extends ASTNode {
    def typeVal(): TypeIdentifier
}

sealed trait ExprNode extends RValueNode

case class ArrayLiterNode(exprList: List[ExprNode]) extends RValueNode {
    override def typeVal(): TypeIdentifier = {
        if (!exprList.isEmpty) {
            val elemType = exprList(0).typeVal()
            elemType match {
                case ArrayIdentifier(base, dim) => ArrayIdentifier(base, dim + 1)
                case t => ArrayIdentifier(t, 1)
            }
        } else {
            /* type for empty array wiil be assigned at variable definition */
            ArrayIdentifier(AnyIdentifier(), 1)
        }
    }

    override def semanticCheck(): Unit = {
        if (!exprList.isEmpty) {
            exprList.map(expr => expr.semanticCheck())
            val exprTypes:List[TypeIdentifier] = exprList.map(expr => expr.typeVal())
            // check whether array literal expr all have same types
            exprTypes
            .zip(exprTypes.map(ty => ty.typeEquals(exprTypes(0))))
            .map{case (x:TypeIdentifier, y:Boolean) => if (!y) {
                Error.addSemErr(s"array literal expr should have type " +
                                s"${exprTypes(0).toString()}, but was ${x}")
            }}
        }
    }
}

case class NewPairNode(fstExpr: ExprNode, sndExpr: ExprNode) extends RValueNode {
    override def typeVal() = {
        PairIdentifier(fstExpr.typeVal(), sndExpr.typeVal())
    }
    override def semanticCheck(): Unit = {
        fstExpr.semanticCheck()
        sndExpr.semanticCheck()
    }
}

case class CallNode(ident: IdentNode, argList: ArgListNode) extends RValueNode {
    var newName: Option[String] = None
    var returnType: Option[TypeIdentifier] = None
    override def typeVal() = returnType match {
        case Some(ty) => ty
        case _ => AnyIdentifier()
    }

    override def semanticCheck(): Unit = {
        argList.callName = ident.name
        argList.semanticCheck()

        val exprTypes: List[TypeIdentifier] = argList.exprList.map(expr => expr.typeVal())
        newName = SemanticChecker.symbolTable.getFuncNewName(ident.name, exprTypes, typeVal())
        newName match {
            case None => Error.addSemErr("Function name \"" + ident.name + 
                                            "\" is not defined or parameter/return type(s) do not match")
            case Some(name) => ident.newName = name
        }
    }
}

case class ArgListNode(exprList: List[ExprNode]) extends ASTNode {
    var callName = ""
    override def semanticCheck(): Unit = {
        for (e <- exprList) {
            e.semanticCheck()
            /* abstract type semantic error: function call arguement cannot be abstract */
            if (e.typeVal().isRepacable()) {
                val name = e match {
                    case i: IdentNode => s" \"${i.name}\" "
                    case _ => " "
                }
                Error.addSemErr(s"function call to ${callName}: parameter${name}cannot be of abstract type")
            }
        }
    }
}

sealed trait TypeNode extends ASTNode {
    def typeVal(): TypeIdentifier = AnyIdentifier()
    def typeStrVal(): String = typeVal().toString()
}

// Including int, char, string, bool
case class BaseTypeNode(ty: String) extends TypeNode with PairElemTypeNode {
    override def typeVal() = {
        ty match {
            case "int" => IntIdentifier()
            case "bool" => BoolIdentifier()
            case "char" => CharIdentifier()
            case "string" => StrIdentifier()
            /* syntactic error: int, bool, char, string are the only basic types */
            case _ =>
                throw new IllegalArgumentException(s"basic type: \"${ty}\" is not a legal basic type")
       }
    }
}

case class ArrayTypeNode(ty: TypeNode) extends TypeNode with PairElemTypeNode {
    /* temporary holder of the basetype of an array */
    var arrayType: TypeIdentifier = AnyIdentifier()
    var arrayDim = 1

    override def typeVal(): TypeIdentifier = {
        return ArrayIdentifier(getType(ty).typeVal(), countDimension(ty))
    }

    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        /* abstract type semantic error: array type cannot be abstract */
        if (ty.typeVal().isRepacable()) {
            Error.addSemErr("array type cannot be abstact type")
        }
        arrayType = getType(ty).typeVal()
        arrayDim = countDimension(ty)
    }

    def countDimension(ty: TypeNode): Int = {
        ty match {
            case ArrayTypeNode(arrayTy) => 1 + countDimension(arrayTy)
            case _ => 1
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
case class PairTypeNode(fstPET: PairElemTypeNode, sndPET: PairElemTypeNode) extends PairElemTypeNode with TypeNode {
    override def typeVal(): TypeIdentifier = {
        PairIdentifier(fstPET.typeVal(), sndPET.typeVal())
    }

    override def typeStrVal(): String = {
        s"pair(${fstPET.typeStrVal()}, ${sndPET.typeStrVal()})"
    }

    override def semanticCheck(): Unit = {
        fstPET.semanticCheck()
        sndPET.semanticCheck()
    }
}

sealed trait PairElemTypeNode extends TypeNode {
    def typeVal(): TypeIdentifier
}

/* "pair" can be refering to any valid pair type */
case class PETPairNode() extends PairElemTypeNode {
    override def typeVal(): TypeIdentifier = PairIdentifier(AnyIdentifier(), AnyIdentifier())
    override def typeStrVal(): String = "pair"
}

case class IntLiterNode(n: Int) extends ExprNode {
    override def typeVal() = IntIdentifier()
}

case class BoolLiterNode(b: Boolean) extends ExprNode {
    override def typeVal() = BoolIdentifier()
}

case class CharLiterNode(c: Char) extends ExprNode {
    override def typeVal() = CharIdentifier()
}

case class StrLiterNode(s: String) extends ExprNode {
    override def typeVal() = StrIdentifier()
}

// Example: null
case class PairLiterNode() extends ExprNode {
    override def typeVal() = NullIdentifier()
}

sealed trait UnOpExprNode extends ExprNode

// bool -> bool
case class NotNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = BoolIdentifier()
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        if(!SemanticChecker.typeCheck(BoolIdentifier(), expr.typeVal())) {
            Error.addSemErr(s"Unary op: expression unexpected type " +
                            s"${expr.typeVal().toString()}, expected type bool")
        }
    }
}

// int -> int
case class NegNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = IntIdentifier()
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        if(!SemanticChecker.typeCheck(IntIdentifier(), expr.typeVal())) {
            Error.addSemErr(s"Unary op: expression unexpected type " +
                            s"${expr.typeVal().toString()}, expected type int")
        }
    }
}

// array -> int
case class LenNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = IntIdentifier()
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        expr.typeVal() match {
            case a: ArrayIdentifier => 
            case _ => Error.addSemErr("Wrong array dimension in len(), " +
                                      "expected at least 1-dimensional array")
        }
    }
}

// char -> int
case class OrdNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = IntIdentifier()
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        if(!SemanticChecker.typeCheck(CharIdentifier(), expr.typeVal())) {
            Error.addSemErr(s"Unary op: expression unexpected type " +
                            s"${expr.typeVal().toString()}, expected type char")
        }
    }
}

// int -> char
case class ChrNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = CharIdentifier()
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        if(!SemanticChecker.typeCheck(IntIdentifier(), expr.typeVal())) {
            Error.addSemErr(s"Unary op: expression unexpected type " +
                            s"${expr.typeVal().toString()}, expected type int")
        }
    }
}

sealed trait BinOpExprNode extends ExprNode {
    override def typeVal(): TypeIdentifier = {
        this match {
            case MulNode(_,_) | DivNode(_,_) | ModNode(_,_) | AddNode(_,_) | SubNode(_,_) => IntIdentifier()
            case _ => BoolIdentifier()
            }
    }
    override def semanticCheck(): Unit = {
        typeVal()
        this match {
            case MulNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type int")
                    }
                    if (! SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int")
                    }
            }
            case DivNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type int")
                    }
                    if (! SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int")
                    }
            }
            case ModNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type int")
                    }
                    if (! SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int")
                    }
            }
            case AddNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type int")
                    }
                    if (! SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int")
                    }
            }
            case SubNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type int")
                    }
                    if (! SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int")
                    }
            }
            case GTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal()) &&
                        !SemanticChecker.typeCheck(CharIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type int/char")
                    }
                    if (!SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal()) && 
                        !SemanticChecker.typeCheck(CharIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int/char")
                    }
            }
            case GTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal()) && 
                        !SemanticChecker.typeCheck(CharIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type int/char")
                    }
                    if (!SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal()) && 
                        !SemanticChecker.typeCheck(CharIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int/char")
                    }
            }
            case LTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal()) && 
                        !SemanticChecker.typeCheck(CharIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type int/char")
                    }
                    if (!SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal()) && 
                        !SemanticChecker.typeCheck(CharIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int/char")
                    }
            }
            case LTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(IntIdentifier(), fstExpr.typeVal()) && 
                        !SemanticChecker.typeCheck(CharIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int/char")
                    }
                    if (!SemanticChecker.typeCheck(IntIdentifier(), sndExpr.typeVal()) && 
                        !SemanticChecker.typeCheck(CharIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type int/char")
                    }
            }
            case EqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(fstExpr.typeVal(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: wrong type, expected ${fstExpr.typeVal()} " +
                                        s"instead of ${sndExpr.typeVal()}")
                    }
            }
            case IEqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(fstExpr.typeVal(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: wrong type, expected ${fstExpr.typeVal()}" +
                                        s"instead of ${sndExpr.typeVal()}")
                    }
            }
            case AndNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck(BoolIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " +
                                        s"${fstExpr.typeVal().toString()}, expected lhs type bool")
                    }
                    if (!SemanticChecker.typeCheck(BoolIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " +
                                        s"${sndExpr.typeVal().toString()}, expected lhs type bool")
                    }
            }
            case OrNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck(BoolIdentifier(), fstExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: first expression unexpected type " + 
                                        s"${fstExpr.typeVal().toString()}, expected lhs type bool")
                    }
                    if (! SemanticChecker.typeCheck(BoolIdentifier(), sndExpr.typeVal())) {
                        Error.addSemErr(s"Binary op: second expression unexpected type " + 
                                        s"${sndExpr.typeVal().toString()}, expected lhs type bool")
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