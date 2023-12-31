package wacc

import scala.collection.mutable.{LinkedHashMap}
import wacc.Constants._
import wacc.Constants.StdFuncsEnum._
import wacc.AssignmentTranslations._
import StandardFuncs._
import ControlFlowGraph.nextTRNum

object CodeGenerator {
    /* .data directive in mainFunc stores all string declarations */
    val mainFunc = FuncBlock()
    mainFunc.setGlobalMain()
    mainFunc.name = "main"

    
    var currInstBlock = mainFunc.body
    
    /* List of functions defined or in use in the program */
    val controlFlowFuncs = LinkedHashMap[String, FuncBlock]()

    def stringDef(string: String): String = {
        mainFunc.directive.addTextLabelToData(string)
    }

    /* translate functions */

    /*  Main entry point for the AST to IR1 (CFG),
        translating the main program body and functions from ProgramNode. */
    def translateAST(p: ProgramNode): Unit = {
        translateMain(p.stat)
        for (func <- p.funcList) {
            translate(func)
        }
    }

    /*  Main body translation,
        appending global .data strings and push/pops of registers. */
    def translateMain(stat: StatNode): Unit = {

        assert(currInstBlock == mainFunc.body)
        // Callee-saved register pushes
        currInstBlock.addInst(
            PushInst(fp, lr),
            PushInst(r4, r5, r6, r7, r8, r10, r12),
            MovInst(fp, sp)
        )

        translate(stat)

        // Program exiting with code 0
        currInstBlock.addInst(
            MovInst(r0, ImmVal(0)),
            MovInst(sp, fp),
            PopInst(r4, r5, r6, r7, r8, r10, r12),
            PopInst(fp, pc)
        )

        controlFlowFuncs.addOne(mainFunc.name, mainFunc)
    }

    /*  FuncNode translation,
        handles naming, callee-save and saving into FuncBlock,
        which is used to separate the code when printing assembly. */
    def translate(f: FuncNode): Unit = {
        val funcBlock = FuncBlock()
        funcBlock.paramList = f.paramList.paramList
        /* change current instruction block to func block */
        currInstBlock = funcBlock.body
        // Callee-saved register pushes
        currInstBlock.addInst(
            PushInst(fp, lr),
            PushInst(r4, r5, r6, r7, r8, r10, r12),
            MovInst(fp, sp)
        )

        // As front-end checked for all return paths,
        // return pop instructions can and should be placed
        // at return node translations.
        translate(f.stat)

        // Naming all non-main functions as wacc_*ident*
        funcBlock.name = if (funcBlock.GLOBAL_MAIN) "main" else f.ident.newName
        controlFlowFuncs.addOne(funcBlock.name, funcBlock)
    }

    // Case-matching into StatNode subclasses
    def translate(node: StatNode): Unit = {
        node match {
            case n: SkipNode => translate(n: SkipNode)
            case n: AssignIdentNode => translate(n: AssignIdentNode)
            case n: LValuesAssignNode => translate(n: LValuesAssignNode)
            case n: ReadNode => translate(n: ReadNode)
            case n: FreeNode => translate(n: FreeNode)
            case n: ReturnNode => translate(n: ReturnNode)
            case n: ExitNode => translate(n: ExitNode)
            case n: PrintNode => translate(n: PrintNode)
            case n: PrintlnNode => translate(n: PrintlnNode)
            case n: IfNode => translate(n: IfNode)
            case n: WhileNode => translate(n: WhileNode)
            case n: BeginEndNode => translate(n: BeginEndNode)
            case n: StatJoinNode => translate(n: StatJoinNode)
        }
    }

    def translate(node: SkipNode): Unit = { /* No instruction */ }
    
    /*  Variable initialisations,
        translate RHS, assigning to the register assigned to the value,
        and pop back caller-saved registers if RHS is a function call. */
    def translate(node: AssignIdentNode): Unit = {
        val op = transRVal(node.rvalue)
        currInstBlock.addInst(MovInst(Variable(node.ident.newName), op))

        // Pop back Caller Regs
        node.rvalue match {
            case CallNode(_, argList) => {
                if (argList.exprList.length >= 4) {
                    currInstBlock.addInst(PopInst(r0, r1, r2, r3))
                    currInstBlock.addInst(AddInst(sp, sp, ImmVal(4 * (argList.exprList.length - 4))))
                } else {
                    for (c <- 0 to argList.exprList.length - 1) {
                        currInstBlock.addInst(PopInst(FixedRegister(c)))
                    }
                }
            }
            case _ => 
        }
    }

    /*  Assignment statements to initialised variables,
        translating RHS as initialisation does, and storing them
        either in registers, stack or on the heap. */
    def translate(node: LValuesAssignNode): Unit = {
        // Process right value first and store it in a temp reg
        val op = transRVal(node.rvalue)
        node.lvalue match {
            case i: IdentNode => {
                currInstBlock.addInst(MovInst(Variable(i.newName), op))
            }
            case a: ArrayElemNode => {
                // Loading array addresses until it reaches final index,
                // then store the evaluated RHS into the offsetted address.
                currInstBlock.addInst(
                    MovInst(r8, op),
                    PushInst(r8),
                    MovInst(r8, Variable(a.ident.newName)),
                    PushInst(r8))
                for (arrayNum <- 1 to a.exprList.length - 1) {
                    currInstBlock.addInst(
                        // Ready for special convention for _arrLoad,
                        // r8 for array addr, r10 for index
                        PushInst(r8),
                        MovInst(r10, translate(a.exprList(arrayNum))),
                        PopInst(r8),
                        BranchInst(ARRAY_LOAD_LABEL, link=true)
                    )
                    setUsed(ArrLdr)
                }

                currInstBlock.addInst(
                    // Ready for special convention for _arrStore,
                    // r8 for the value to be stored, r9 for array addr, r10 for index
                    MovInst(r10, translate(a.exprList.last)),
                    PopInst(r9),
                    PopInst(r8),
                    a.typeVal() match {
                        case CharIdentifier() => BranchInst(ARRAY_STORE_B_LABEL, link=true)
                        case _ => BranchInst(ARRAY_STORE_LABEL, link=true)
                    }
                )
                setUsed(ArrStrb)
                setUsed(ArrStr)
            }
            case FstNode(lvalue) => storeToPairElemAddr(0, lvalue, op)
            case SndNode(lvalue) => storeToPairElemAddr(4, lvalue, op)
        }
        // Pop back Caller Regs
        node.rvalue match {
            case CallNode(_, argList) => {
                if (argList.exprList.length >= 4) {
                    currInstBlock.addInst(PopInst(r0, r1, r2, r3))
                    currInstBlock.addInst(AddInst(sp, sp, ImmVal(4 * (argList.exprList.length - 4))))
                } else {
                    for (c <- 0 to argList.exprList.length - 1) {
                        currInstBlock.addInst(PopInst(FixedRegister(c)))
                    }
                }
            }
            case _ => 
        }
    }

    /*  Reading into memory. */
    def translate(node: ReadNode): Unit = {
        var retOp: Register = TempRegister(nextTRNum())
        node.lvalue match {
            case i: IdentNode => {
                retOp = Variable(i.newName)
                SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get
            }
            case a: ArrayElemNode => {
                retOp = Variable(a.ident.newName)
                SemanticChecker.symbolTable.lookUpVarNewName(a.ident.newName).get
            }   
            case f: FstNode => {
                f.lvalue match {
                    case i: IdentNode => {
                        retOp = Variable(i.newName)
                        SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get

                        /* null check */
                        currInstBlock.addInst(
                            CmpInst(retOp, ImmVal(0)),
                            BranchInst(NULL_POINTER_LABEL, link=true, condition=LessOrEqual())
                        )
                        setUsed(NullErr)
                    }
                    case _ => f.lvalue.typeVal()
                }
            }
            case s: SndNode => {
                s.lvalue match {
                    case i: IdentNode => {
                        retOp = Variable(i.newName)
                        SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get

                        /* null check */
                        currInstBlock.addInst(
                            CmpInst(retOp, ImmVal(0)),
                            BranchInst(NULL_POINTER_LABEL, link=true, condition=LessOrEqual())
                        )
                        setUsed(NullErr)
                    }
                    case _ => s.lvalue.typeVal()
                }
            }
        }

        node.lvalue.typeVal() match {
            case CharIdentifier() => IOFunc.readChar(retOp)
            case IntIdentifier() => IOFunc.readInt(retOp)
            case _ => throw new IllegalArgumentException("error: reading non char / int")
        }
    }
    /*  Freeing addresses allocated in the memory. 
        Segmentation fault if op translated is not */
    def translate(node: FreeNode): Unit = {
        val op = translate(node.expr)
        val exprTy = node.expr match {
            case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get
            case a: ArrayElemNode => SemanticChecker.symbolTable.lookUpVarNewName(a.ident.newName).get
            case _ => node.expr.typeVal()
        }
        exprTy match {
            case PairIdentifier(ty1, ty2) => {
                currInstBlock.addInst(
                    MovInst(r0, op),
                    BranchInst(FREE_PAIR_LABEL, link=true)
                )
                setUsed(FreeP)
            }
            case ArrayIdentifier(baseTy, dim) => {
                currInstBlock.addInst(
                    // If the variable is on the stack
                    // its translated and put into r8
                    SubInst(r0, r8, ImmVal(4)),
                    BranchInst("free", link=true)
                )
            }
            case _ => {
                currInstBlock.addInst(
                    MovInst(r0, op),
                    BranchInst("free", link=true)
                )
            }
        }
    }

    /*  Return from function,
        moving return value to r0 and handle callee-saved register pops,
        then pop back program counter to return to previous address. */
    def translate(node: ReturnNode): Unit = {
        val op = translate(node.expr)
        currInstBlock.addInst(MovInst(Constants.r0, op),
            MovInst(sp, fp),
            PopInst(r4, r5, r6, r7, r8, r10, r12),
            PopInst(fp, pc))
    }
    
    /*  Exit from program,
        branching to exit c-function to halt the program
        with provided exit code. */
    def translate(node: ExitNode): Unit = {
        val op = translate(node.expr)
        currInstBlock.addInst(
            MovInst(r0, op),
            BranchInst("exit", link=true)
        )
    }
     
    /*  Printing statements. */
    def translate(node: PrintNode): Unit = {
        currInstBlock.addInst(PushInst(r0, r1, r2, r3))
        val retOp = translate(node.expr)
        // Find the type to print
        node.expr match {
            case i: IdentNode => {
                val exprTy = SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get
                exprTy match {
                    case ArrayIdentifier(CharIdentifier(),_) => IOFunc.printString(retOp)
                    case ArrayIdentifier(_,_) => IOFunc.printPtr(retOp)
                    case _ => printType(exprTy, retOp)
                }
            }
            case a: ArrayElemNode => {
                val exprTy = SemanticChecker.symbolTable.lookUpVarNewName(a.ident.newName).get
                printType(exprTy, retOp)
            }
            case _ => {
                val exprTy = node.expr.typeVal()
                printType(exprTy, retOp)
            }
        }
        currInstBlock.addInst(PopInst(r0, r1, r2, r3))
    }

    def printType(ty: TypeIdentifier, retOp: Operand): Unit = {
        ty match {
            case CharIdentifier() => IOFunc.printChar(retOp)
            case IntIdentifier() | ArrayIdentifier(IntIdentifier(),_) => IOFunc.printInt(retOp)
            case StrIdentifier() | ArrayIdentifier(CharIdentifier(),_) => IOFunc.printString(retOp)
            case BoolIdentifier() | ArrayIdentifier(BoolIdentifier(),_) => IOFunc.printBool(retOp)
            case _ => IOFunc.printPtr(retOp)
        }
    }

    /*  Print empty line. */
    def translate(node: PrintlnNode): Unit = {
        translate(PrintNode(node.expr))

        currInstBlock.addInst(PushInst(r0, r1, r2, r3))
        IOFunc.println()
        currInstBlock.addInst(PopInst(r0, r1, r2, r3))
    }

    /*  If condition branching,
        creates TRUE and FALSE instruction blocks and link to the current
        instruction block, then processes the condition and add branching
        to FALSE block, keeping TRUE block directly under code before if. */
    def translate(node: IfNode): Unit = {
        val ifBlock = IfBlock()
        /* next block of current control flow graph block points to this ifBlock */
        val ifTrue = ifBlock.nextT
        val ifFalse = ifBlock.nextF
        val next = ifBlock.next
        val op = translate(node.expr)
        op match {
            case ImmVal(num) => {
                currInstBlock.addInst(
                    MovInst(r8, op),
                    CmpInst(r8, immTrue)
                )
            }
            case r: Register => {
                currInstBlock.addInst(CmpInst(r, immTrue))
            }
            case _ => throw new UnsupportedOperationException("If condition cannot be evaluated")
        }
        /* branch to false if false */
        currInstBlock.addInst(BranchNumInst(ifFalse.num, condition=NotEqual()))
        currInstBlock.next = ifBlock
        currInstBlock = ifTrue

        translate(node.fstStat)
        currInstBlock.addInst(BranchNumInst(next.num))
        currInstBlock = ifFalse

        translate(node.sndStat)
        currInstBlock = next
    }

    /*  While loop,
        condition is first evaluated and if satisfied,
        enter loop section below, which will branch back to the condition
        once the loop is completed.
        If the condition is not satisfied, the program will jump to the code
        block right after the while loop. */
    def translate(node: WhileNode): Unit = {
        val whileBlock = WhileBlock()
        val cond = whileBlock.cond
        val loop = whileBlock.loop
        val next = whileBlock.next

        currInstBlock.next = whileBlock
        currInstBlock = cond
        
        val op = translate(node.expr)
        op match {
            case ImmVal(num) => {
                currInstBlock.addInst(
                    MovInst(r8, op),
                    CmpInst(r8, immTrue)
                )
            }
            case r: Register => {
                currInstBlock.addInst(CmpInst(r, immTrue))
            }
            case _ => throw new UnsupportedOperationException("While condition cannot be evaluated")
        }
        currInstBlock.addInst(BranchNumInst(next.num, condition=NotEqual()))
        currInstBlock = loop
        translate(node.stat)
        currInstBlock.addInst(BranchNumInst(cond.num))
        currInstBlock = next
    }
    def translate(node: BeginEndNode): Unit = {
        translate(node.stat)
    }
    def translate(node: StatJoinNode): Unit = {
        for (n <- node.statList) {
            translate(n)
        }
    }

    /*  Expression translations:
        Literals => literal values
        String => added to .data section
        ArrayElem => load from heap
        Unary and Binary Operators
        =>  evaluate and add instructions for calculation,
            storing in r8 as intermediate and return. */
    def translate(node: ExprNode): Operand = {
        def translateComparisonOperators(op1: Register, op2: Operand, cond: Condition, notcond: Condition): Register = {
            currInstBlock.addInst(
                CmpInst(op1, op2),
                MovInst(r8, immTrue, condition=cond),
                MovInst(r8, immFalse, condition=notcond),
                FreeRegister(op1)
            )
            r8
        }
        
        node match {
            case IntLiterNode(n) => {
                if (n >= 0 && n <= 255) {ImmVal(n)}
                else {
                    currInstBlock.addInst(LdrPseudoInst(r8, n))
                    r8
                }
            }
            case BoolLiterNode(true) => ImmVal(1)
            case BoolLiterNode(false) => ImmVal(0)
            case CharLiterNode(c) => {
                val num = c.toInt
                ImmVal(num)
            }
            case StrLiterNode(s) => {
                val sList = s.split("").map(x => replaceChar(x))
                val newStr = sList.fold("")((x: String, y: String) => s"${x}${y}")

                val label = stringDef(newStr)
                val loadlabelAddrInstr = LabelAddress(label)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(LdrInst(reg, loadlabelAddrInstr))
                reg
            }            
            case PairLiterNode() => ImmVal(0)
            case i: IdentNode => Variable(i.newName)
            case ArrayElemNode(ident, exprList) => {
                val op1 = translate(ident)
                currInstBlock.addInst(MovInst(r8, op1))
                for (expr <- exprList) {
                    currInstBlock.addInst(PushInst(r8))
                    val op2 = translate(expr)
                    currInstBlock.addInst(
                        MovInst(r10, op2),
                        PopInst(r8),
                        SemanticChecker.symbolTable.lookUpVarNewName(ident.newName) match {
                            case Some(ArrayIdentifier(ty, _)) => {
                                ty match {
                                case CharIdentifier() => BranchInst(ARRAY_LOAD_B_LABEL, link=true)
                                case _ => BranchInst(ARRAY_LOAD_LABEL, link=true)
                                }
                            }
                            case _ => throw new UnsupportedOperationException("array type mismatched?")
                        }
                    )
                    setUsed(ArrLdr)
                    setUsed(ArrLdrB)
                }
                r8
            }
            case NotNode(expr) => {
                var op = translate(expr)
                op match {
                    case ImmVal(num) => {
                        if (num == 0) op = immTrue
                        else op = immFalse
                        op 
                    }
                    case r: Register => {
                        currInstBlock.addInst(
                            CmpInst(r, ImmVal(1)),
                            MovInst(r8, ImmVal(1), condition=NotEqual()),
                            MovInst(r8, ImmVal(0), condition=Equal())
                        )
                        r8
                    }
                    case _ => throw new UnsupportedOperationException("Not node evaluation error")
                }            
            }
            case NegNode(expr) => {
                val op = translate(expr)
                op match {
                    case ImmVal(num) => {
                        currInstBlock.addInst(
                            MovInst(r8, op), 
                            RsbsInst(r8, r8, ImmVal(0)),
                            BranchInst(OVERFLOW_LABEL, link=true, condition=Overflow())
                        )
                    }
                    case r: Register => {
                        currInstBlock.addInst(
                            RsbsInst(r8, r, ImmVal(0)),
                            BranchInst(OVERFLOW_LABEL, link=true, condition=Overflow())
                        )
                    }
                    case _ => throw new UnsupportedOperationException("Neg node evaluation error")
                }
                setUsed(OverflowErr)
                r8
            }
            case LenNode(expr) => {
                val op = translate(expr)
                val offset = ImmOffset(op.asInstanceOf[Register], ARRAY_LENGTH_OFFSET)
                currInstBlock.addInst(LdrInst(r8, offset))
                r8
            }
            case OrdNode(expr) => {
                val op = translate(expr)
                op match {
                    case _: ImmVal | _: Register => {
                        op
                    }
                    case _ => throw new UnsupportedOperationException("Ord node evaluation error")
                }
            }
            case ChrNode(expr) => {
                val op = translate(expr)
                op match {
                    case _: ImmVal | _: Register => {
                        op
                    }
                    case _ => throw new UnsupportedOperationException("Chr node evaluation error")
                }
            }
            case MulNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                var reg2: Register = r9
                op2 match {
                    case ImmVal(num) => {
                    currInstBlock.addInst(MovInst(reg2, op2))
                    }
                    case r: Register => {
                        reg2 = r
                    }
                    case _ => throw new UnsupportedOperationException("Mul node op2 evaluation error")
                }
                currInstBlock.addInst(
                    SmullInst(r8, r9, reg1, reg2),
                    CmpInst(r9, ASR(r8, ImmVal(INT_HIGHEST_BIT))),
                    BranchInst(OVERFLOW_LABEL, condition=NotEqual()),
                    FreeRegister(reg1)
                )
                setUsed(OverflowErr)
                r8
            }
            case DivNode(fstexpr, sndexpr) => {
                currInstBlock.addInst(PushInst(r0, r1, r2, r3))
                val op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(r0, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(
                    MovInst(r1, op2),
                    CmpInst(r1, ImmVal(0)),
                    BranchInst(ZERO_DIVISION_LABEL, condition=Equal()),
                    BranchInst("__aeabi_idivmod", link=true),
                    MovInst(r8, r0),
                    PopInst(r0, r1, r2, r3)
                )
                setUsed(ZeroDivErr)
                r8
            }
            case ModNode(fstexpr, sndexpr) => {
                currInstBlock.addInst(PushInst(r0, r1, r2, r3))
                val op1 = translate(fstexpr)
                currInstBlock.addInst(MovInst(r0, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(
                    MovInst(r1, op2),
                    CmpInst(r1, ImmVal(0)),
                    BranchInst(ZERO_DIVISION_LABEL, condition=Equal()),
                    BranchInst("__aeabi_idivmod", link=true),
                    MovInst(r8, r1),
                    PopInst(r0, r1, r2, r3)
                )
                setUsed(ZeroDivErr)
                r8
            }
            case AddNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(
                    AddsInst(r8, reg, op2),
                    BranchInst(OVERFLOW_LABEL, link=true, condition=Overflow()),
                    FreeRegister(reg)
                )
                setUsed(OverflowErr)
                r8
            }
            case SubNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(
                    SubsInst(r8, reg, op2),
                    BranchInst(OVERFLOW_LABEL, link=true, condition=Overflow()),
                    FreeRegister(reg)
                )
                setUsed(OverflowErr)
                r8
            }
            case GTNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, GreaterThan(), LessOrEqual())
            }
            case GTENode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, GreaterOrEqual(), LessThan())
            }
            case LTNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, LessThan(), GreaterOrEqual())
            }
            case LTENode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, LessOrEqual(), GreaterThan())
            }
            case EqNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, Equal(), NotEqual())
            }
            case IEqNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, NotEqual(), Equal())
            }
            case AndNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                val reg2 = TempRegister(nextTRNum())
                val newBlock = InstBlock()
                currInstBlock.addInst(
                    MovInst(reg2, op2),
                    CmpInst(reg1, immTrue), 
                    BranchNumInst(newBlock.num, condition=NotEqual()),
                    CmpInst(reg2, immTrue)
                )
                currInstBlock.next = newBlock
                currInstBlock = newBlock

                currInstBlock.addInst(
                    MovInst(r8, immTrue, condition=Equal()),
                    MovInst(r8, immFalse, condition=NotEqual()),
                    FreeRegister(reg1),
                    FreeRegister(reg2)
                )
                r8
            }
            case OrNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister(nextTRNum())
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                val reg2 = TempRegister(nextTRNum())
                val newBlock = InstBlock()
                currInstBlock.addInst(
                    MovInst(reg2, op2),
                    CmpInst(reg1, immTrue), 
                    BranchNumInst(newBlock.num, condition=Equal()),
                    CmpInst(reg2, immTrue)
                )
                currInstBlock.next = newBlock
                currInstBlock = newBlock
                
                currInstBlock.addInst(
                    MovInst(r8, immTrue, condition=Equal()),
                    MovInst(r8, immFalse, condition=NotEqual()),
                    FreeRegister(reg1),
                    FreeRegister(reg2)
                )
                r8
            }
        }
    }

    // append forward slash character to special character to print in program file
    def replaceChar(s: String): String = {
        var ret = s
        if (ESCAPE_CHAR_LIST.contains(s)) {
            ret = "\\u".replace("u", s)
        }
        ret
    }
}
