package wacc

import parsley.genericbridges

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

sealed trait ASTNode {
    def semanticCheck(): Unit = {

    }
}

case class ProgramNode(funcList: List[FuncNode], stat: StatNode) extends ASTNode {

    override def semanticCheck(): Unit = {
        for (f <- funcList) {
            f.semanticCheck()
        }
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

    // ! Check all execution paths contain return or exit
    override def semanticCheck(): Unit = {
        
        ty.semanticCheck()
        ident.semanticCheck()
        paramList.semanticCheck()

        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1
        stat.semanticCheck()
        SemanticChecker.scopeStack.pop()

        var paramtypeList = ListBuffer[String]()
        for (param <- paramList.paramList) {
            // Check for repeated parameter names
            
            if (SemanticChecker.tableContainsIdentifier(param.ident)) {
                paramtypeList += param.ty.typeVal()
                SemanticChecker.symbolTable.addVar(param.ident.name, param.ty.typeVal())
            }
        }

        // Check ident is used
        SemanticChecker.symbolTable.lookUpFunc(ident.name) match {
            case Some(n) => {
                SemanticChecker.errorMessage += "Function name \"" + ident.name + "\" is already used\n"
                false
            }
            // Add to symbol table
            case _ => SemanticChecker.symbolTable.addFunc(ident.name, paramtypeList.toList, ty.typeVal())
        }
        
        // Check return type
        stat match {
            case s: StatJoinNode => { 
                    for (stat <- s.statList) {
                        stat match {
                            case ReturnNode(expr) 
                                => SemanticChecker.typeCheck(ty, expr)
                            case _ => // ! Find all return nodes
                        }
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
        ident.semanticCheck()
        rvalue.semanticCheck()
        if (SemanticChecker.symbolTable.checkVarDefined(ident.name)){
            SemanticChecker.errorMessage += "Variable name \"" + ident.name + "\" is already used in the same scope\n"
        }
        else {
            if (SemanticChecker.typeCheck(ty, rvalue)) {
                ty match {
                    case b: BaseTypeNode => SemanticChecker.symbolTable.addVar(ident.name, ty.typeVal())
                    case _ =>
                }
            }
        }

        rvalue match{
            case id: IdentNode => SemanticChecker.tableContainsIdentifier(id)
            case a: ArrayElemNode => SemanticChecker.tableContainsIdentifier(a.ident)
            // ! case f: FstNode =>
            // ! case f: SndNode =>
            case _ =>
        }
        SemanticChecker.typeCheck(ty, rvalue)

    }
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

        lvalue match{
            case id: IdentNode => SemanticChecker.tableContainsIdentifier(id)
            case a: ArrayElemNode => SemanticChecker.tableContainsIdentifier(a.ident)
            // ! case f: FstNode =>
            // ! case f: SndNode =>
            case _ =>
        }
        rvalue match{
            case id: IdentNode => SemanticChecker.tableContainsIdentifier(id)
            case a: ArrayElemNode => SemanticChecker.tableContainsIdentifier(a.ident)
            // ! case f: FstNode =>
            // ! case f: SndNode =>
            case _ =>
        }
        SemanticChecker.typeCheck(lvalue, rvalue)
    }
}

case class ReadNode(lvalue: LValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        val ty = lvalue.typeVal()
        if (ty.contains("-")) {  // ! pair or wrong fst/snd
            SemanticChecker.errorMessage += "Wrong type in read\n"
        }
        else if (ty != "int" && ty != "char") {
            SemanticChecker.errorMessage += "Wrong type in read\n"
        }
    }
}

case class FreeNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        val ty = expr.typeVal()
        // Check if type is array or pair
        if (!(ty.contains(":") || ty.contains("-"))) {
            SemanticChecker.errorMessage += "Wrong type in free\n"
        }
    }
}

case class ReturnNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
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
        if (expr.typeVal() != "int") {
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
        
        if (expr.typeVal() != "bool") {
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

        if (expr.typeVal() != "bool") {
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
    def typeVal(): String = "any"
}

case class IdentNode(name: String) extends LValueNode with ExprNode { 
    //left for testing purpose will delete later
    // var symbolTableName = s"PLACEHODER--0!${name}"

    override def typeVal() = {
        val identifier = SemanticChecker.symbolTable.lookUpVar(name)
        identifier match {
            case Some(VarIdentifier(ty)) => ty
            case Some(ArrayIdentifier(ty, dim, size, elements)) => ty + ":" + dim.toString()
            case Some(PairIdentifier(ty1, ty2)) => ty1 + "-" + ty2
            case _ => "ERROR"
        }
    }
    
    override def semanticCheck(): Unit = {
        typeVal()
    }
}

// Example: a[1][b]
case class ArrayElemNode(ident: IdentNode, exprList: List[ExprNode]) extends LValueNode with ExprNode {
    
    override def typeVal() = "TBC"
    override def semanticCheck(): Unit = {
        typeVal()
        var elemDim = 0
            for (e <- exprList) {
                elemDim += 1
                e.semanticCheck()
                if (e.typeVal() != "int") {
                    SemanticChecker.errorMessage += s"array elem index: unexpected type ${e.typeVal()}, expected int\n"
                }
            }
            /* ERROR: will not stay when everyting abstracted to concrete type identifier */
            // if (elemDim > typeVal.charAt(typeVal.length - 2).toInt) {
            //     SemanticChecker.errorMessage += s"array elem type: unexpected type ${ident.typeVal}:${elemDim}, expected ${arrIdentType}\n"
            // }
    }
}

sealed trait PairElemNode extends LValueNode with RValueNode {
    override def typeVal() = "-"
}

case class FstNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal() = lvalue.typeVal()
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        typeVal()
    }
}

case class SndNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal() = lvalue.typeVal()
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        typeVal()
    }
}

// RValueNode
sealed trait RValueNode extends ASTNode {
    def typeVal(): String = "any"
}

sealed trait ExprNode extends RValueNode

// Example: [1,a] (a=2) / [a,b] (a=[1,2],b=[3,4])
case class ArrayLiterNode(exprList: List[ExprNode]) extends RValueNode {
    val size = exprList.size
    override def typeVal() = {
        if (!exprList.isEmpty) {
            val exprTypes = exprList.map(expr => {
                expr.typeVal()
            })
            exprTypes
            .map(ty => ty == exprTypes(0))
            .fold(true)((x,y) => {
                val equals = x == y
                if (!equals) {
                    SemanticChecker.errorMessage += s"array literal expr sould have type ${x}, but was ${y}" 
                }
                equals
            })

            var dim = 1
            val arrayPattern: Regex = "[a-z]+:[0-9]+".r
            /* ERROR: will not stay when everyting abstracted to concrete type identifier */
            // if (!arrayPattern.matches(exprTypes(0))) {
            //     //exprTypes(0).length - 2
            //     //dim += exprTypes(0).charAt(exprTypes(0).length - 2).toInt
            // } else {
                exprTypes(0) + ":" + dim
            } else {
            /* ERROR: will not stay when everyting abstracted to concrete type identifier */
                "any:1"
            }
    
        
    }
    override def semanticCheck(): Unit = {
        exprList.map(expr => expr.typeVal())

    }
}

// Example: newpair(1,a)
case class NewPairNode(fstExpr: ExprNode, sndExpr: ExprNode) extends RValueNode {
    override def typeVal() = {
        fstExpr.typeVal() + "-" + sndExpr.typeVal()
    }
    override def semanticCheck(): Unit = {
        fstExpr.semanticCheck()
        sndExpr.semanticCheck()
        typeVal()
    }
}


case class CallNode(ident: IdentNode, argList: ArgListNode) extends RValueNode {
    override def typeVal() = {
        val lookUp = SemanticChecker.symbolTable.lookUpFunc(ident.name)
        if (lookUp == None) {
            "ERROR"
        }
        else {
            val identifier = lookUp.get
            identifier match {
                case f: FuncIdentifier => f.returntype
                case _ => "ERROR"
            }
        }

    }
    override def semanticCheck(): Unit = {
        ident.semanticCheck()
        argList.semanticCheck()
        typeVal()

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
                            if (arg.typeVal() != f.paramtype.apply(index)) {
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
    def typeVal(): String = "any"
}

case class BaseTypeNode(ty: String) extends TypeNode with PairElemTypeNode {
    override def typeVal() = ty
}

// Example: int[]
case class ArrayTypeNode(ty: TypeNode) extends TypeNode with PairElemTypeNode {
    override def typeVal(): String = {
        val dimension = countDimension(ty)
        val baseType = getType(ty)
        baseType.typeVal() + ":" + dimension
    }
    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        typeVal()
    }

    def countDimension(ty: TypeNode): Int = {
        ty match {
            case ArrayTypeNode(arrayTy) => 1 + countDimension(arrayTy)
            case node => 1
        }  
    }

    def getType(ty: TypeNode): TypeNode = {
        ty match {
            case ArrayTypeNode(arrayTy) => getType(arrayTy)
            case node => node
        }  
    }
}

// Example: Pair(bool, int[])
case class PairTypeNode(fstPET: PairElemTypeNode, sndPET: PairElemTypeNode) extends TypeNode {
    override def typeVal() = "pair"
    override def semanticCheck(): Unit = {
        fstPET.semanticCheck()
        sndPET.semanticCheck()
        typeVal()
    }
}

// PairElemTypeNode
sealed trait PairElemTypeNode extends ASTNode

case class PETPairNode() extends PairElemTypeNode

case class IntLiterNode(n: Int) extends ExprNode {
    override def typeVal() = "int"
    override def semanticCheck(): Unit = {
        typeVal()
    }
}

case class BoolLiterNode(b: Boolean) extends ExprNode {
    override def typeVal() = "bool"
    override def semanticCheck(): Unit = {
        typeVal()
    }
}

case class CharLiterNode(c: Char) extends ExprNode {
    override def typeVal() = "char"
    override def semanticCheck(): Unit = {
        typeVal()
    }
}

case class StrLiterNode(s: String) extends ExprNode {
    override def typeVal() = "string"
    override def semanticCheck(): Unit = {
        if (s.contains("\n")) {
            SemanticChecker.errorMessage += "String cannot contain newline\n"
        }
        typeVal()
    }
}

case class PairLiterNode() extends ExprNode

sealed trait UnOpExprNode extends ExprNode

case class NotNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = "bool"
    override def semanticCheck(): Unit = SemanticChecker.basicTypeCheck("bool", expr)
}

case class NegNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = "int"
    override def semanticCheck(): Unit = SemanticChecker.basicTypeCheck("int", expr)
}

case class LenNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = "int"
    override def semanticCheck(): Unit = {
        /* arrays have their types in the form of: <baseType>:<dimension> */
        val arrayPattern: Regex = "[a-z]+:[0-9]+".r
        if (!arrayPattern.matches(expr.typeVal())) {
            SemanticChecker.errorMessage += "Type error: expected at least 1-dimensional array.\n"
        }
    }
}

case class OrdNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = "int"
    override def semanticCheck(): Unit = SemanticChecker.basicTypeCheck("char", expr)
}

case class ChrNode(expr: ExprNode) extends UnOpExprNode {
    override def typeVal() = "char"
    override def semanticCheck(): Unit = SemanticChecker.basicTypeCheck("int", expr)
}

sealed trait BinOpExprNode extends ExprNode {
    override def typeVal(): String = {
        this match {
            case MulNode(_,_) | DivNode(_,_) | ModNode(_,_) | AddNode(_,_) | SubNode(_,_) => "int"
            case _ => "bool"
            }
    }
    override def semanticCheck(): Unit = {
        this match {
            case MulNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", fstExpr)
                    }
            }
            case DivNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", fstExpr)
                    }
            }
            case ModNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", fstExpr)
                    }
            }
            case AddNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", fstExpr)
                    }
            }
            case SubNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", fstExpr)
                    }
            }
            case GTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", "char", fstExpr)
                    }
            }
            case GTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", "char", fstExpr)
                    }
            }
            case LTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", "char", fstExpr)
                    }
            }
            case LTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("int", "char", fstExpr)
                    }
            }
            case EqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
            }
            case IEqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
            }
            case AndNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("bool", fstExpr)
                    }
            }
            case OrNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: unexpected rhs type ${fstExpr.typeVal()}, expected lhs type ${sndExpr.typeVal()}\n"
                    }
                    else {
                        SemanticChecker.basicTypeCheck("bool", fstExpr)
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
