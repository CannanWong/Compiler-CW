package wacc

import scala.collection.mutable.{LinkedHashMap}
import wacc.Constants._
import wacc.Constants.StdFuncsEnum._
import wacc.AssignmentTranslations._
import StandardFuncs._

object CodeGenerator {
    /* .data directive stores all string declarations */
    val mainFunc = FuncBlock()
    mainFunc.setGlobalMain()
    mainFunc.name = "main"

    // var controlFlowGraph = FuncBlock()
    
    var currInstBlock = mainFunc.body
    
    /* NEW: temporory design to accomodate label jumps */
    val controlFlowFuncs = LinkedHashMap[String, FuncBlock]()

        /* utility functions */
    // def switchCurrInstrBlock(newFuncBlock: FuncBlock, instBlock: InstBlock): Unit = {
    //     controlFlowGraph = newFuncBlock
    //     currInstBlock = instBlock
    //     controlFlowGraph.currBlock = instBlock        
    // }

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
        /* resets control graph since instantiaing CodeGenerator will increase val in CFG */
        // ControlFlowGraph.resetCFG()
        
        // val mainFuncBlock = FuncBlock()
        // mainFuncBlock.setGlobalMain()
        // mainFuncBlock.name = "main"

        // mainFunc = mainFuncBlock
        /* change current instruction block to func block */
        // switchCurrInstrBlock(mainFunc, mainFunc.currBlock)

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
        // switchCurrInstrBlock(funcBlock, funcBlock.currBlock)
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
        funcBlock.name = if (funcBlock.GLOBAL_MAIN) "main" else s"wacc_${f.ident.name}"
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
                currInstBlock.addInst(PopInst(r0, r1, r2, r3))
                if (argList.exprList.length > 4)
                    currInstBlock.addInst(AddInst(sp, sp, ImmVal(4 * (argList.exprList.length - 4))))
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
            case ArrayElemNode(ident, exprList) => {
                // Loading array addresses until it reaches final index,
                // then store the evaluated RHS into the offsetted address.
                currInstBlock.addInst(
                    MovInst(r8, op),
                    PushInst(r8),
                    MovInst(r8, Variable(ident.newName)),
                    PushInst(r8))
                for (arrayNum <- 1 to exprList.length - 1) {
                    currInstBlock.addInst(
                        // Ready for special convention for _arrLoad,
                        // r8 for array addr, r10 for index
                        PushInst(r8),
                        MovInst(r10, translate(exprList(arrayNum))),
                        PopInst(r8),
                        BranchLinkInst("_arrLoad")
                    )
                    setUsed(ArrLdr)
                }

                currInstBlock.addInst(
                    // Ready for special convention for _arrStore,
                    // r8 for the value to be stored, r9 for array addr, r10 for index
                    MovInst(r10, translate(exprList.last)),
                    PopInst(r9),
                    PopInst(r8),
                    SemanticChecker.symbolTable.lookUpVarNewName(ident.newName) match {
                        case Some(CharIdentifier()) => BranchLinkInst("_arrStoreB")
                        case _ => BranchLinkInst("_arrStore")
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
                currInstBlock.addInst(PopInst(r0, r1, r2, r3))
                if (argList.exprList.length > 4)
                    currInstBlock.addInst(AddInst(sp, sp, ImmVal(4 * (argList.exprList.length - 4))))
            }
            case _ => 
        }
    }

    /*  Reading into memory. */
    def translate(node: ReadNode): Unit = {
        var retOp: Register = new TempRegister()
        val exprTy = node.lvalue match {
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
                    }
                    case _ => f.lvalue.typeVal()
                }
            }
            case s: SndNode => {
                s.lvalue match {
                    case i: IdentNode => {
                        retOp = Variable(i.newName)
                        SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get
                    }
                    case _ => s.lvalue.typeVal()
                }
            }
        }
        exprTy match {
            case CharIdentifier() => IOFunc.readChar(retOp)
            case IntIdentifier() => IOFunc.readInt(retOp)
            case _ => {
                currInstBlock.addInst(WaccComment("read creceives type: " + exprTy))
                val tempreg1 = TempRegister()
                currInstBlock.addInst(MovInst(tempreg1, ImmVal(1)))
                IOFunc.readInt(tempreg1)
            }
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
                    BranchLinkInst("_freePair")
                )
                setUsed(FreeP)
            }
            case ArrayIdentifier(baseTy, dim) => {
                currInstBlock.addInst(
                    // If the variable is on the stack
                    // its translated and put into r8
                    // TODO: Check if ident is consistent in this case
                    SubInst(r0, r8, ImmVal(4)),
                    BranchLinkInst("free")
                )
            }
            case _ => {
                currInstBlock.addInst(
                    MovInst(r0, op),
                    BranchLinkInst("free")
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
            BranchLinkInst("exit")
        )
    }
     
    /*  Printing statements. */
    def translate(node: PrintNode): Unit = {
        currInstBlock.addInst(PushInst(r0, r1, r2, r3))
        val retOp = translate(node.expr)
        val exprTy = node.expr match {
            case i: IdentNode => SemanticChecker.symbolTable.lookUpVarNewName(i.newName).get
            case a: ArrayElemNode => SemanticChecker.symbolTable.lookUpVarNewName(a.ident.newName).get
            case _ => node.expr.typeVal()
        }
        exprTy match {
            case CharIdentifier() => IOFunc.printChar(retOp)
            case IntIdentifier() => IOFunc.printInt(retOp)
            case StrIdentifier() => IOFunc.printString(retOp)
            case BoolIdentifier() => IOFunc.printBool(retOp)
            case ArrayIdentifier(CharIdentifier(),_) => IOFunc.printString(retOp)
            case ArrayIdentifier(IntIdentifier(),_) => IOFunc.printInt(retOp)
            case ArrayIdentifier(BoolIdentifier(),_) => IOFunc.printBool(retOp)
            case ArrayIdentifier(_,_) => IOFunc.printPtr(retOp)
            case PairIdentifier(_,_) => IOFunc.printPtr(retOp)
            // anyIdentifier or null
            case _ => IOFunc.printPtr(retOp)
        }
        currInstBlock.addInst(PopInst(r0, r1, r2, r3))
    }

    /*  Print empty line. */
    def translate(node: PrintlnNode): Unit = {
        translate(new PrintNode(node.expr))

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
        currInstBlock.addInst(BranchNumCondInst(NOT_EQUAL, ifFalse.num))
        currInstBlock.next = ifBlock
        currInstBlock = ifTrue
        // switchCurrInstrBlock(controlFlowGraph, ifTrue)
        translate(node.fstStat)
        currInstBlock.addInst(BranchNumInst(next.num))
        currInstBlock = ifFalse
        // switchCurrInstrBlock(controlFlowGraph, ifFalse)
        translate(node.sndStat)
        currInstBlock = next
        // switchCurrInstrBlock(controlFlowGraph, next)

        // controlFlowFuncs += ((ifTrue.num.toString, ifTrue),
        // (ifFalse.num.toString, ifFalse),
        // (next.num.toString, next))
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
        currInstBlock.addInst(BranchNumCondInst("ne", next.num))
        currInstBlock = loop
        translate(node.stat)
        currInstBlock.addInst(BranchNumInst(cond.num))
        currInstBlock = next
        // switchCurrInstrBlock(controlFlowGraph, next)
        // controlFlowFuncs += ((cond.num.toString, cond),
        // (loop.num.toString, loop),
        // (next.num.toString, next))
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
        def translateComparisonOperators(op1: Register, op2: Operand, cond: String, notcond: String): Register = {
            currInstBlock.addInst(
                CmpInst(op1, op2),
                MovCondInst(cond, r8, immTrue),
                MovCondInst(notcond, r8, immFalse),
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
                for (ch <- ESCAPE_CHAR_LIST) {
                    s.replaceAll(ch, s"\\${ch}")    
                }               
                val label = stringDef(s)
                val loadlabelAddrInstr = LabelAddress(label)
                val reg = TempRegister()
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
                                case CharIdentifier() => BranchLinkInst("_arrLoadB")
                                case _ => BranchLinkInst("_arrLoad")
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
                            MovCondInst(NOT_EQUAL, r8, ImmVal(1)),
                            MovCondInst( EQUAL, r8, ImmVal(0))
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
                            NegInst(r8, r8)
                        )
                    }
                    case r: Register => {
                        currInstBlock.addInst(NegInst(r8, r))
                    }
                    case _ => throw new UnsupportedOperationException("Neg node evaluation error")
                }
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
                val reg1 = TempRegister()
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
                    BranchCondInst(NOT_EQUAL, "_errOverflow"),
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
                    BranchCondInst( EQUAL, "_errDivZero"),
                    BranchLinkInst("__aeabi_idivmod"),
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
                    BranchCondInst( EQUAL, "_errDivZero"),
                    BranchLinkInst("__aeabi_idivmod"),
                    MovInst(r8, r1),
                    PopInst(r0, r1, r2, r3)
                )
                setUsed(ZeroDivErr)
                r8
            }
            case AddNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(
                    AddsInst(r8, reg, op2),
                    BranchLinkCondInst(OVERFLOW, "_errOverflow"),
                    FreeRegister(reg)
                )
                setUsed(OverflowErr)
                r8
            }
            case SubNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                currInstBlock.addInst(
                    SubsInst(r8, reg, op2),
                    BranchLinkCondInst(OVERFLOW, "_errOverflow"),
                    FreeRegister(reg)
                )
                setUsed(OverflowErr)
                r8
            }
            case GTNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, GREATER_THAN, LESS_OR_EQUAL)
            }
            case GTENode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, GREATER_OR_EQUAL, LESS_THAN)
            }
            case LTNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, LESS_THAN, GREATER_OR_EQUAL)
            }
            case LTENode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, LESS_OR_EQUAL, GREATER_THAN)
            }
            case EqNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, EQUAL, NOT_EQUAL)
            }
            case IEqNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg = TempRegister()
                currInstBlock.addInst(MovInst(reg, op1))
                val op2 = translate(sndexpr)
                translateComparisonOperators(reg, op2, NOT_EQUAL, EQUAL)
            }
            // TODO: fix label
            case AndNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister()
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                val reg2 = TempRegister()
                val newBlock = InstBlock()
                currInstBlock.addInst(
                    MovInst(reg2, op2),
                    CmpInst(reg1, immTrue), 
                    BranchNumCondInst(NOT_EQUAL, newBlock.num),
                    CmpInst(reg2, immTrue)
                )
                currInstBlock.next = newBlock
                currInstBlock = newBlock
                // switchCurrInstrBlock(controlFlowGraph, newBlock)

                currInstBlock.addInst(
                    MovCondInst( EQUAL, r8, immTrue),
                    MovCondInst(NOT_EQUAL, r8, immFalse),
                    FreeRegister(reg1),
                    FreeRegister(reg2)
                )
                // controlFlowFuncs += ((newBlock.num.toString, newBlock))
                r8
            }
            case OrNode(fstexpr, sndexpr) => {
                val op1 = translate(fstexpr)
                val reg1 = TempRegister()
                currInstBlock.addInst(MovInst(reg1, op1))
                val op2 = translate(sndexpr)
                val reg2 = TempRegister()
                val newBlock = InstBlock()
                currInstBlock.addInst(
                    MovInst(reg2, op2),
                    CmpInst(reg1, immTrue), 
                    BranchNumCondInst( EQUAL, newBlock.num),
                    CmpInst(reg2, immTrue)
                )
                currInstBlock.next = newBlock
                currInstBlock = newBlock
                // switchCurrInstrBlock(controlFlowGraph, newBlock)
                currInstBlock.addInst(
                    MovCondInst( EQUAL, r8, immTrue),
                    MovCondInst(NOT_EQUAL, r8, immFalse),
                    FreeRegister(reg1),
                    FreeRegister(reg2)
                )
                // controlFlowFuncs += ((newBlock.num.toString, newBlock))
                r8
            }
        }
    }
}
