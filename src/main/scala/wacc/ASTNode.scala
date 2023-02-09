package wacc
import scala.collection.mutable.ListBuffer

sealed trait ASTNode {
    def semanticCheck(): Unit = {

    }
}

case class ProgramNode(funcList: List[FuncNode], stat: StatNode) extends ASTNode {

    override def semanticCheck(): Unit = {
        for (f <- funcList) {
            f.semanticCheck()
        }
        SemanticChecker.insideFunc = false
        stat.semanticCheck()
    }
}

case class FuncNode(ty: TypeNode, ident: IdentNode, paramList: ParamListNode, stat: StatNode) extends ASTNode {
    
    /**
      * Checks for:
        1. legal params
        2. legal stats
        3. legal type (tbc)
        4. leagal func ident
        5. return type matches function type
      */
    override def semanticCheck(): Unit = {
        
        ty.semanticCheck()
        ident.semanticCheck()
        paramList.semanticCheck()

        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        stat.semanticCheck()
        SemanticChecker.scopeStack.pop()

        var paramtypeList = ListBuffer[Identifier]()
        for (param <- paramList.paramList) {
            // Check for repeated parameter names
            
            if (SemanticChecker.tableContainsIdentifier(param.ident)) {
                paramtypeList += param.ty.typeVal1()
                SemanticChecker.symbolTable.addVar(param.ident.name, param.ty.typeVal1())
            }
        }

        // Check ident is used
        SemanticChecker.symbolTable.lookUpFunc(ident.name) match {
            case Some(n) => {
                SemanticChecker.errorMessage += "Function name \"" + ident.name + "\" is already used\n"
            }
            // Add to symbol table
            case _ => SemanticChecker.symbolTable.addFunc(ident.name, paramtypeList.toList, ty.typeVal1())
        }
        
        // Check that return type matches function return type
        checkReturnType(ty, stat)
    }

    def checkReturnType(ty: TypeNode, stat: StatNode): Unit = {
        stat match {
            case r: ReturnNode => {
            if (!SemanticChecker.typeCheck1(ty, r.expr)) {
                    SemanticChecker.errorMessage += s"func type mismatch: expected ${ty.typeVal1().toString}, gets ${r.expr.typeVal1().toString()}\n"
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
        ident.semanticCheck()
    }
}

// StatNode
sealed trait StatNode extends ASTNode

case class SkipNode() extends StatNode

case class AssignIdentNode(ty: TypeNode, ident: IdentNode, rvalue: RValueNode) extends StatNode {

    /**
     * Checks for:
       1. ident's name not declared in scope
       2. RHS val and type consistent 
       3. rval is semantically correct (TODO)
       4. lhs type is not func (type check should catch)
      */

    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        //ident.semanticCheck()
        rvalue.semanticCheck()

        if (SemanticChecker.symbolTable.checkVarDefined(ident.name)){
            SemanticChecker.errorMessage += "Variable name \"" + ident.name + "\" is already used in the same scope\n"
        }
        else {
            ty match {
                case b: BaseTypeNode => SemanticChecker.symbolTable.addVar(ident.name, b.typeVal1())
                case a: ArrayTypeNode => SemanticChecker.symbolTable.addArray(ident.name, a.arrayType, a.arrayDim)
                case p: PairTypeNode => SemanticChecker.symbolTable.addPair(ident.name, p.fstPET.typeVal1(), p.sndPET.typeVal1())
                case _ =>
            }
        }

        rvalue match{
            case id: IdentNode => {
                SemanticChecker.tableContainsIdentifier(id)
                if (!SemanticChecker.typeCheck1(ty, id)) {
                    SemanticChecker.errorMessage += s"assignment type mismatch: expected ${ty.typeVal1().toString}, gets ${id.typeVal1().toString()}"
                }
            }
            case e => {
                if (!SemanticChecker.typeCheck1(ty, e)) {
                    SemanticChecker.errorMessage += s"assignment type mismatch: expected ${ty.typeVal1().toString}, gets ${e.typeVal1().toString()}"
                }
            }
        }
    }

    // DEBUG: lhs rhs typecheck did not check for anything
    //    SemanticChecker.typeCheck(ty, rvalue)
}

// Example: a=5
case class LValuesAssignNode(lvalue: LValueNode, rvalue: RValueNode) extends StatNode {
    /**
      * check for:
        1. reassignment to itself (TODO pairs)
        2. lhs type is not func (type check should catch)
      */
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        rvalue.semanticCheck()

        // var lhsArrayType = ""
        // var lhsArrayDim = 0

        // lvalue match{
        //     case id: IdentNode => SemanticChecker.tableContainsIdentifier(id)
        //     case a: ArrayElemNode => {
        //         lhsArrayType = a.arrayType
        //         lhsArrayDim = a.arrayDim
        //     }
        //     // ! case f: FstNode =>
        //     // ! case f: SndNode =>
        //     case _ =>
        // }
        // rvalue match{
        //     case id: IdentNode => SemanticChecker.tableContainsIdentifier(id)
        //     case a: ArrayElemNode => {
        //         if (lhsArrayType != a.arrayType) {
        //             SemanticChecker.errorMessage += "Wrong type of array declaration\n"
        //         }ty
        //         if (lhsArrayDim != a.arrayDim) {
        //             SemanticChecker.errorMessage += "Wrong dimension of array declaration\n"
        //         }
        //     }
        //     // ! case f: FstNode =>
        //     // ! case f: SndNode =>
        //     case _ =>
        // }

        // check whether lhs is an illegal assignment to a func
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
        
        if (!SemanticChecker.typeCheck1(lvalue, rvalue)) {
                SemanticChecker.errorMessage += s"assignment: type mismatch: expected ${lvalue.typeVal1().toString}, gets ${rvalue.typeVal1().toString()}"
        }
    }
}

case class ReadNode(lvalue: LValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        val ty = lvalue.typeVal1()
        ty match {
            case a: IntIdentifier =>
            case p: CharIdentifier => 
            case _ => SemanticChecker.errorMessage += "Wrong type in read\n"
        }
    }
}

case class FreeNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        val ty = expr.typeVal1()
        // Check if type is array or pair
        ty match {
            case a: ArrayIdentifier =>
            case p: PairIdentifier =>
            case _ => SemanticChecker.errorMessage += "Wrong type in free\n"
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
        if (!SemanticChecker.typeCheck1(new IntIdentifier, expr)) {
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
        
        if (!SemanticChecker.typeCheck1(new BoolIdentifier, expr)) {
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

        if (!SemanticChecker.typeCheck1(new BoolIdentifier, expr)) {
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
    def typeVal1(): Identifier
}

case class IdentNode(name: String) extends LValueNode with ExprNode { 
    //left for testing purpose will delete later
    // var symbolTableName = s"PLACEHODER--0!${name}"

    override def typeVal1() = {
        val identifier = SemanticChecker.symbolTable.lookUpVar(name)
        identifier match {
            case Some(ty) => ty
            case _ => new AnyIdentifier
        }
    }
    
    override def semanticCheck(): Unit = {
        typeVal1()
    }
}

// Example: a[1][b]
case class ArrayElemNode(ident: IdentNode, exprList: List[ExprNode]) extends LValueNode with ExprNode {
    var arrayType = new AnyIdentifier
    var arrayDim = 0
    override def typeVal1() = {
         val identifier = SemanticChecker.symbolTable.lookUpVar(ident.name)
        identifier match {
            case Some(ArrayIdentifier(baseTy: AnyIdentifier, dim: Int)) => {
                if (arrayDim > 0) {
                    new ArrayIdentifier(baseTy, arrayDim)
                } else if (arrayDim == 0) {
                    baseTy
                } else {
                    new AnyIdentifier
                }
            }
            case _ => new AnyIdentifier
        }
     }

    override def semanticCheck(): Unit = {
        if (SemanticChecker.tableContainsIdentifier(ident)) {
            val identifier = SemanticChecker.symbolTable.lookUpVar(ident.name)
            identifier match {
                case (a@Some(ArrayIdentifier(baseTy: AnyIdentifier, dim: Int))) => {
                    arrayType = new ArrayIdentifier(baseTy, exprList.size)
                    arrayDim = dim

                    for (e <- exprList) {
                        arrayDim -= 1
                        e.semanticCheck()
                        if (!SemanticChecker.typeCheck1(new IntIdentifier, e)) {
                            SemanticChecker.errorMessage += s"array elem index: unexpected type ${e.typeVal1().toString()}, expected int\n"
                        }
                    }
                    if (arrayDim < 0) {
                        SemanticChecker.errorMessage += 
                            s"array type error: unexpected type ${(new ArrayIdentifier(baseTy, exprList.length)).toString()}, expected ${a.value.toString()}\n"
                    }
                }
                case _ => SemanticChecker.errorMessage += s"variable ${ident.name} is not in scope\n"
            }
        }
        typeVal1()
    }
}

sealed trait PairElemNode extends LValueNode with RValueNode {
}

case class FstNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal1() = lvalue.typeVal1()
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        typeVal1()
    }
}

case class SndNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal1() = lvalue.typeVal1()
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        typeVal1()
    }
}

// RValueNode
sealed trait RValueNode extends ASTNode {
    def typeVal1(): Identifier
}

sealed trait ExprNode extends RValueNode

// Example: [1,a] (a=2) / [a,b] (a=[1,2],b=[3,4])
case class ArrayLiterNode(exprList: List[ExprNode]) extends RValueNode {
    override def typeVal1(): Identifier = {
        if (!exprList.isEmpty) {
            val elemType = exprList(0).typeVal1()
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
            val exprTypes:List[Identifier] = exprList.map(expr => expr.typeVal1())
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
    override def typeVal1() = {
        new PairIdentifier(fstExpr.typeVal1(), sndExpr.typeVal1())
    }
    override def semanticCheck(): Unit = {
        fstExpr.semanticCheck()
        sndExpr.semanticCheck()
        typeVal1()
    }
}


case class CallNode(ident: IdentNode, argList: ArgListNode) extends RValueNode {
    override def typeVal1() = {
        val lookUp = SemanticChecker.symbolTable.lookUpFunc(ident.name)
        if (lookUp == None) {
            new AnyIdentifier
        }
        else {
            val identifier = lookUp.get
            identifier match {
                case f: FuncIdentifier => f.returntype
                case _ =>  new AnyIdentifier
            }
        }
    }
    override def semanticCheck(): Unit = {
        ident.semanticCheck()
        argList.semanticCheck()
        typeVal1()

        // Check if function is declared
        val lookUp = SemanticChecker.symbolTable.lookUpFunc(ident.name)
        if (lookUp == None) {
            SemanticChecker.errorMessage += "Function name \"" + ident.name + "\" is not defined in this scope\n"
        }
        // Check if arg list is of correct type and number
        else {
            val identifier = lookUp.get
            identifier match {
                case f: FuncIdentifier => {
                    if (argList.exprList.length != f.paramtype.length) {
                        SemanticChecker.errorMessage += "Function argument number is different from function parameter number\n"
                    }
                    else {
                        var index = 0
                        for (arg <- argList.exprList) {
                            if (!arg.typeVal1().typeEquals(f.paramtype.apply(index))) {
                                SemanticChecker.errorMessage += "Function argument type is incorrect\n"
                            }
                            index += 1
                        }
                    }
                }
                case _ => SemanticChecker.errorMessage += "[Not possible!]"
            }
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
    def typeVal1(): AnyIdentifier = new AnyIdentifier
}

case class BaseTypeNode(ty: String) extends TypeNode with PairElemTypeNode {
    // includes: int, char, string, bool
    override def typeVal1() = {
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
    var arrayType = new AnyIdentifier
    var arrayDim = 1

    override def typeVal1(): AnyIdentifier = {
        return new ArrayIdentifier(getType(ty).typeVal1(), countDimension(ty))
    }

    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        arrayType = getType(ty).typeVal1()
        arrayDim = countDimension(ty)
        typeVal1()
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
    override def typeVal1(): AnyIdentifier = {
        new PairIdentifier(fstPET.typeVal1(), sndPET.typeVal1())
    }

    override def semanticCheck(): Unit = {
        fstPET.semanticCheck()
        sndPET.semanticCheck()
        typeVal1()
    }
}

// PairElemTypeNode
sealed trait PairElemTypeNode extends ASTNode {
    def typeVal1() = new AnyIdentifier
}
// <pair>
case class PETPairNode() extends PairElemTypeNode {
    override def typeVal1(): AnyIdentifier = super[PairElemTypeNode].typeVal1()
}

case class IntLiterNode(n: Int) extends ExprNode {
    override def typeVal1() = new IntIdentifier
}

case class BoolLiterNode(b: Boolean) extends ExprNode {
    override def typeVal1() = new BoolIdentifier
}

case class CharLiterNode(c: Char) extends ExprNode {
    override def typeVal1() = new CharIdentifier
}

case class StrLiterNode(s: String) extends ExprNode {
    override def typeVal1() = new StrIdentifier
    override def semanticCheck(): Unit = {
        if (s.contains("\n")) {
            SemanticChecker.errorMessage += "String cannot contain newline\n"
        }
    }
}

case class PairLiterNode() extends ExprNode {
    // val: null
    override def typeVal1() = new AnyIdentifier
}

sealed trait UnOpExprNode extends ExprNode

// bool -> bool
case class NotNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal1() = new BoolIdentifier
    override def semanticCheck(): Unit = {
        if(!SemanticChecker.typeCheck1(new BoolIdentifier, expr)) {
            SemanticChecker.errorMessage += s"Unary op: expression unexpected type ${expr.typeVal1().toString()}, expected type bool\n"
        }
    }
    // override def semanticCheck(): Unit = SemanticChecker.basicTypeCheck("bool", expr)
}

// int -> int
case class NegNode(expr: ExprNode) extends UnOpExprNode {
     override def typeVal1() = new IntIdentifier
    override def semanticCheck(): Unit = {
        if(!SemanticChecker.typeCheck1(new IntIdentifier, expr)) {
            SemanticChecker.errorMessage += s"Unary op: expression unexpected type ${expr.typeVal1().toString()}, expected type int\n"
        }
    }
    //override def semanticCheck(): Unit = SemanticChecker.basicTypeCheck("int", expr)
}

// array -> int
case class LenNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal1() = new IntIdentifier
    override def semanticCheck(): Unit = {
        expr.typeVal1() match {
            case a: ArrayIdentifier => 
            case _ => SemanticChecker.errorMessage += "Type error: expected at least 1-dimensional array.\n"
        }
    }
}

// char -> int
case class OrdNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal1() = new IntIdentifier
    override def semanticCheck(): Unit = {
        if(!SemanticChecker.typeCheck1(new CharIdentifier, expr)) {
            SemanticChecker.errorMessage += s"Unary op: expression unexpected type ${expr.typeVal1().toString()}, expected type char\n"
        }
    }
    //override def semanticCheck(): Unit = SemanticChecker.basicTypeCheck("char", expr)
}

// int -> char
case class ChrNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal1() = new CharIdentifier
    override def semanticCheck(): Unit = {
        if(!SemanticChecker.typeCheck1(new IntIdentifier, expr)) {
            SemanticChecker.errorMessage += s"Unary op: expression unexpected type ${expr.typeVal1().toString()}, expected type int\n"
        }
    }
    //override def semanticCheck(): Unit = SemanticChecker.basicTypeCheck("int", expr)
}

sealed trait BinOpExprNode extends ExprNode {
    override def typeVal1(): Identifier = {
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
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
            }
            case DivNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
            }
            case ModNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
            }
            case AddNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
            }
            case SubNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int\n"
                    }
            }
            case GTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck1(new IntIdentifier, fstExpr) || !SemanticChecker.typeCheck1(new CharIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int/char\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr) || !SemanticChecker.typeCheck1(new CharIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int/char\n"
                    }
            }
            case GTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck1(new IntIdentifier, fstExpr) || !SemanticChecker.typeCheck1(new CharIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int/char\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr) || !SemanticChecker.typeCheck1(new CharIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int/char\n"
                    }
            }
            case LTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck1(new IntIdentifier, fstExpr) || !SemanticChecker.typeCheck1(new CharIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int/char\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr) || !SemanticChecker.typeCheck1(new CharIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int/char\n"
                    }
            }
            case LTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (!SemanticChecker.typeCheck1(new IntIdentifier, fstExpr) || !SemanticChecker.typeCheck1(new CharIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type int/char\n"
                    }
                    if (! SemanticChecker.typeCheck1(new IntIdentifier, sndExpr) || !SemanticChecker.typeCheck1(new CharIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: second expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type int/char\n"
                    }
            }
            case EqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal1().typeEquals(sndExpr.typeVal1())) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal1().toString()}, expected lhs type ${sndExpr.typeVal1().toString()}\n"
                    }
            }
            case IEqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal1().typeEquals(sndExpr.typeVal1())) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${sndExpr.typeVal1().toString()}, expected lhs type ${sndExpr.typeVal1().toString()}\n"
                    }
            }
            case AndNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck1(new BoolIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type bool\n"
                    }
                    if (! SemanticChecker.typeCheck1(new BoolIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type bool\n"
                    }
            }
            case OrNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (! SemanticChecker.typeCheck1(new BoolIdentifier, fstExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${fstExpr.typeVal1().toString()}, expected lhs type bool\n"
                    }
                    if (! SemanticChecker.typeCheck1(new BoolIdentifier, sndExpr)) {
                        SemanticChecker.errorMessage += s"Binary op: first expression unexpected type ${sndExpr.typeVal1().toString()}, expected lhs type bool\n"
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
