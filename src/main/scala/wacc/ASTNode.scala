package wacc

import parsley.genericbridges

import scala.collection.mutable.ListBuffer

sealed trait ASTNode {
    def semanticCheck(): Unit = {}
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

    override def semanticCheck(): Unit = {
        
        ty.semanticCheck()
        ident.semanticCheck()

        // Check if function name is used
        val funcNameUsed = SemanticChecker.symbolTable.lookUpFunc(ident.name) match {
            case Some(n) => {
                SemanticChecker.errorMessage += "Function name \"" + ident.name + "\" is already used\n"
                true
            }
            case _ => false
        }

        SemanticChecker.scopeStack.push(SemanticChecker.nextScope)
        SemanticChecker.nextScope += 1

        paramList.semanticCheck()

        var paramtypeList = ListBuffer[String]()
        for (param <- paramList.paramList) {
            // Check for repeated parameter names
            if (SemanticChecker.symbolTable.checkVarDefined(param.ident.name)) {
                SemanticChecker.errorMessage += "Repeated parameter name " + param.ident.name + "\n"
            }
            else {
                paramtypeList += param.ty.typeVal()
                SemanticChecker.symbolTable.addVar(param.ident.name, param.ty.typeVal())
            }
        }
        // Add to symbol table
        if (!funcNameUsed) {
            SemanticChecker.symbolTable.addFunc(ident.name, paramtypeList.toList, ty.typeVal())
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
                SemanticChecker.typeCheck(ty, r.expr)
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
            case _ => // ! array pair
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

// Example: int a = 5
case class AssignIdentNode(ty: TypeNode, ident: IdentNode, rvalue: RValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        ident.semanticCheck()
        rvalue.semanticCheck()
        
        // Check if variable name is already declared in the same scope
        if (SemanticChecker.symbolTable.checkVarDefined(ident.name)){
            SemanticChecker.errorMessage += "Variable name \"" + ident.name + "\" is already used in the same scope\n"
        }
        // Add to symbol table and get type
        else {
            ty match {
                case b: BaseTypeNode => SemanticChecker.symbolTable.addVar(ident.name, b.typeVal())
                case a: ArrayTypeNode => SemanticChecker.symbolTable.addArray(ident.name, a.arrayType(), a.arrayDim())
                case p: PairTypeNode => SemanticChecker.symbolTable.addPair(ident.name, p.fstType(), p.sndType())
            }
        }
        // Type check
        SemanticChecker.typeCheck(ty, rvalue)
    }        
}

// Example: a = 5
case class LValuesAssignNode(lvalue: LValueNode, rvalue: RValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        rvalue.semanticCheck()
        SemanticChecker.typeCheck(lvalue, rvalue)
    }
}

case class ReadNode(lvalue: LValueNode) extends StatNode {
    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        val ty = lvalue.typeVal()
        if (ty != "int" && ty != "char" && ty != "null") {
            SemanticChecker.errorMessage += "Wrong type in read\n"
        }
    }
}

case class FreeNode(expr: ExprNode) extends StatNode {
    override def semanticCheck(): Unit = {
        expr.semanticCheck()
        val ty = expr.typeVal()
        // Check if type is array or pair
        if (ty != "array" && ty != "pair") {
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
            case a: ArrayElemNode => SemanticChecker.tableContainsIdentifier(a.ident)

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
    def typeVal(): String = "ERROR"
    def arrayType(): String = "ERROR"
    def arrayDim(): Int = -1
    def fstType(): String = "ERROR"
    def sndType(): String = "ERROR"
}

case class IdentNode(name: String) extends LValueNode with ExprNode {
    override def typeVal() = {
        SemanticChecker.symbolTable.lookUpVar(name) match {
            case Some(VarIdentifier(ty)) => ty
            case Some(ArrayIdentifier(ty, dim)) => "array"
            case Some(PairIdentifier(ty1, ty2)) => {
                "pair"
            }
            case _ => "ERROR"
        }
    }
    override def arrayType(): String = {
        SemanticChecker.symbolTable.lookUpVar(name) match {
            case Some(ArrayIdentifier(ty,_)) => ty
            case _ => "ERROR"
        }
    }
    override def arrayDim(): Int = {
        SemanticChecker.symbolTable.lookUpVar(name) match {
            case Some(ArrayIdentifier(_,dim)) => dim
            case _ => -1
        }
    }
    override def fstType(): String = {
        SemanticChecker.symbolTable.lookUpVar(name) match {
            case Some(PairIdentifier(ty,_)) => {
                ty
            }
            case _ => "ERROR"
        }
    }
    override def sndType(): String = {
        SemanticChecker.symbolTable.lookUpVar(name) match {
            case Some(PairIdentifier(_,ty)) => {
                ty
            }
            case _ => "ERROR"
        }
    }
    override def semanticCheck(): Unit = {
        typeVal()
        arrayType()
        arrayDim()
        fstType()
        sndType()
    }
}

// Example: a[1][b]
case class ArrayElemNode(ident: IdentNode, exprList: List[ExprNode]) extends LValueNode with ExprNode {
    override def typeVal() = {
        if (arrayDim() > 0) {
            "array"
        }
        else if (arrayDim() == 0) {
            arrayType()
        }
        else {
            SemanticChecker.errorMessage += "Array dimension incorrect\n"
            "ERROR"
        }
    }
    override def arrayType() = ident.arrayType()
    override def arrayDim() = ident.arrayDim() - exprList.length
    override def fstType() = "ERROR"
    override def sndType() = "ERROR"
    override def semanticCheck(): Unit = {
        typeVal()
        arrayType()
        fstType()
        sndType()
        if (SemanticChecker.tableContainsIdentifier(ident)) {
            for (e <- exprList) {
                e.semanticCheck()
                if (e.typeVal() != "int") {
                    SemanticChecker.errorMessage += s"array elem index: unexpected type ${e.typeVal()}, expected int\n"
                }
            }
        }
    }
}

sealed trait PairElemNode extends LValueNode with RValueNode {
    override def typeVal() = "pair"
    override def arrayType() = "ERROR"
    override def arrayDim() = -1
    override def fstType() = "ERROR"
    override def sndType() = "ERROR"
}

case class FstNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal() = {
        if (lvalue.typeVal() == "any") {
            "any"
        }
        else {
            lvalue.fstType()
        }
    }
    override def fstType() = {
        lvalue match {
            case i: IdentNode => {
                if (i.fstType() == "pair") {
                    "any"
                }
                else {
                    "ERROR"
                }
            }
            case f: FstNode => "any"
            case s: SndNode => "any"
            case _ => "ERROR"
        }
    }
    override def sndType() = fstType()

    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        typeVal()
        sndType()
    }
}

case class SndNode(lvalue: LValueNode) extends PairElemNode {
    override def typeVal() = {
        if (lvalue.typeVal() == "any") {
            "any"
        }
        else {
            lvalue.sndType()
        }
    }
    override def fstType() = {
        lvalue match {
            case i: IdentNode => {
                if (i.sndType() == "pair") {
                    "any"
                }
                else {
                    "ERROR"
                }
            }
            case f: FstNode => "any"
            case s: SndNode => "any"
            case _ => "ERROR"
        }
    }
    override def sndType() = fstType()

    override def semanticCheck(): Unit = {
        lvalue.semanticCheck()
        typeVal()
        sndType()
    }
}

// RValueNode
sealed trait RValueNode extends ASTNode {
    def typeVal(): String = "ERROR!"
    def arrayType(): String = "ERROR"
    def arrayDim(): Int = -1
    def fstType(): String = "ERROR"
    def sndType(): String = "ERROR"
}

sealed trait ExprNode extends RValueNode

// Example: [1,a] (a=2) / [a,b] (a=[1,2],b=[3,4])
case class ArrayLiterNode(exprList: List[ExprNode]) extends RValueNode {
    override def typeVal() = "array"
    override def arrayType() = {
        if (!exprList.isEmpty) {
            exprList.head.typeVal()
        }
        else {
            "any"
        }
    }
    override def arrayDim() = {
        if (!exprList.isEmpty) {
            exprList.head match {
                case i: IdentNode => i.arrayDim() + 1
                case a: ArrayElemNode => a.arrayDim() + 1
                case _ => 1
            }
        }
        else {
            1
        }
    }
    override def semanticCheck(): Unit = {
        if (!exprList.isEmpty) {
            val exprTypes = exprList.map(expr => expr.typeVal())
            exprTypes.map(ty => ty == exprTypes(0)).fold(true)(
                (x,y) => {
                        val equals = x == y
                        if (!equals) {
                            SemanticChecker.errorMessage += s"array literal expr should have type ${x}, but was ${y}\n" 
                        }
                        equals
            })
        }
        typeVal()
        arrayType()
        arrayDim()
    }
}

// Example: newpair(1,a)
case class NewPairNode(fstExpr: ExprNode, sndExpr: ExprNode) extends RValueNode {
    override def typeVal() = "pair"
    override def fstType() = fstExpr.typeVal()
    override def sndType() = sndExpr.typeVal()

    override def semanticCheck(): Unit = {
        fstExpr.semanticCheck()
        sndExpr.semanticCheck()
        typeVal()
        fstType()
        sndType()
    }
}

case class CallNode(ident: IdentNode, argList: ArgListNode) extends RValueNode {
    override def typeVal() = {
        SemanticChecker.symbolTable.lookUpFunc(ident.name) match {
            case Some(FuncIdentifier(_,returntype)) => returntype
            case _ => "ERROR!!"
        }
    }
    override def semanticCheck(): Unit = {
        ident.semanticCheck()
        argList.semanticCheck()
        typeVal()

        SemanticChecker.symbolTable.lookUpFunc(ident.name) match {
            // Check if arg list is of correct type and number
            case Some(FuncIdentifier(paramtype,_)) => {
                if (argList.exprList.length != paramtype.length) {
                        SemanticChecker.errorMessage += "Function argument number is different from function parameter number\n"
                    }
                else {
                    var index = 0
                    for (arg <- argList.exprList) {
                        if (arg.typeVal() != paramtype.apply(index)) {
                            SemanticChecker.errorMessage += "Function argument type is incorrect\n"
                        }
                        index += 1
                    }
                }
            }
            case _ => SemanticChecker.errorMessage += "Function name \"" + ident.name + "\" is not defined\n"
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
    def typeVal(): String = "ERROR"
    def arrayType(): String = "ERROR"
    def arrayDim(): Int = -1
    def fstType(): String = "ERROR"
    def sndType(): String = "ERROR"
}

case class BaseTypeNode(ty: String) extends TypeNode with PairElemTypeNode {
    override def typeVal() = ty
    override def fstType() = "ERROR"
    override def sndType() = "ERROR"
}

// Example: int[]
case class ArrayTypeNode(ty: TypeNode) extends TypeNode with PairElemTypeNode {

    override def typeVal(): String = "array"
    override def arrayType() = ty.typeVal()
    override def arrayDim() = countDimension(ty)
    override def fstType() = "ERROR"
    override def sndType() = "ERROR"
    override def semanticCheck(): Unit = {
        ty.semanticCheck()
        typeVal()
        arrayType()
        arrayDim()
        fstType()
        sndType()
    }

    def countDimension(ty: TypeNode): Int = {
        ty match {
            case ArrayTypeNode(arrayTy) => 1 + countDimension(arrayTy)
            case _ => 1
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
    override def fstType() = fstPET.typeVal()
    override def sndType() = sndPET.typeVal()
    override def semanticCheck(): Unit = {
        fstPET.semanticCheck()
        sndPET.semanticCheck()
        typeVal()
        fstType()
        sndType()
    }
}

// PairElemTypeNode
sealed trait PairElemTypeNode extends ASTNode {
    def typeVal() = "ERROR"
    def fstType() = "ERROR"
    def sndType() = "ERROR"
}

case class PETPairNode() extends PairElemTypeNode {
    override def typeVal() = "pair"
    override def fstType() = "any"
    override def sndType() = "any"
}

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

case class PairLiterNode() extends ExprNode {
    override def typeVal(): String = "null"
}

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
        if (expr.typeVal() != "array") {
            SemanticChecker.errorMessage += "Wrong type in len(), expected array\n"
        }
        else {
            if (expr.arrayDim() <= 0) {
                SemanticChecker.errorMessage += "Wrong array dimension in len(), expected at least 1-dimensional array\n"
            }
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
        typeVal()
        this match {
            case MulNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", fstExpr)
                    SemanticChecker.basicTypeCheck("int", sndExpr)
            }
            case DivNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", fstExpr)
                    SemanticChecker.basicTypeCheck("int", sndExpr)
            }
            case ModNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", fstExpr)
                    SemanticChecker.basicTypeCheck("int", sndExpr)
            }
            case AddNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", fstExpr)
                    SemanticChecker.basicTypeCheck("int", sndExpr)
            }
            case SubNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", fstExpr)
                    SemanticChecker.basicTypeCheck("int", sndExpr)
            }
            case GTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", "char", fstExpr)
                    SemanticChecker.basicTypeCheck("int", "char", sndExpr)
            }
            case GTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", "char", fstExpr)
                    SemanticChecker.basicTypeCheck("int", "char", sndExpr)
            }
            case LTNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", "char", fstExpr)
                    SemanticChecker.basicTypeCheck("int", "char", sndExpr)
            }
            case LTENode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("int", "char", fstExpr)
                    SemanticChecker.basicTypeCheck("int", "char", sndExpr)
            }
            case EqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: wrong type, expected ${fstExpr.typeVal()} instead of ${sndExpr.typeVal()}\n"
                    }
            }
            case IEqNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    if (fstExpr.typeVal() != sndExpr.typeVal()) {
                        SemanticChecker.errorMessage += s"Binary op: wrong type, expected ${fstExpr.typeVal()} instead of ${sndExpr.typeVal()}\n"
                    }
            }
            case AndNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("bool", fstExpr)
                    SemanticChecker.basicTypeCheck("bool", sndExpr)
            }
            case OrNode(fstExpr, sndExpr) => {
                    fstExpr.semanticCheck()
                    sndExpr.semanticCheck()
                    SemanticChecker.basicTypeCheck("bool", fstExpr)
                    SemanticChecker.basicTypeCheck("bool", sndExpr)
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