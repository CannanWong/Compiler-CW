package wacc

import wacc.Constants._

object IOFunc {
    val PRINT_INT_LABEL = "_printi"
    val PRINT_STR_LABEL = "_prints"
    val PRINT_CHAR_LABEL = "_printc"
    val PRINT_BOOL_LABEL = "_printb"
    val PRINT_PTR_LABEL = "_printp"
    val PRINTLN_LABEL = "_println"

    val READ_INT_LABEL = "_readi"
    val READ_CHAR_LABEL = "_readc"

    def printEnd(): List[Instruction] = {
      List (
        new BranchLinkInst("printf"),
        new MovInst(r0, new ImmVal(0, new IntIdentifier())),
        new BranchLinkInst("fflush"),
        new PopInst(List(pc))
      )
    }

    def readFunc(label: String, printType: String, funcBlock: FuncBlock): List[Instruction] = {
      val labelStr = funcBlock.directive.addTextLabelToData(printType, label)
      List(
        new PushInst(List(lr)),
        new StrInst(r0, new Offset(sp, arrLenOffset)),
        new MovInst(r1, sp),
        new LdrInst(r0, new LabelAddress(labelStr)),
        new BranchLinkInst("scanf"),
        new LdrInst(r0, new Offset(sp, 0)),
        new AddInst(sp, sp, new ImmVal(arrLenOffset, new IntIdentifier)),
        new PopInst(List(pc))
      )
    }

    def printInt(op: Operand): Unit = {
      /* caller instruction */
      CodeGenerator.currInstBlock.addInst(
        List(
          new MovInst(r0, op),
          new BranchLinkInst(PRINT_INT_LABEL)
          )
      )

      /* callee instruction */
      val printIntFunc = new FuncBlock()
      val text = printIntFunc.directive.addTextLabelToData("%d", PRINT_INT_LABEL)
      printIntFunc.body.addInst(
        List(
          new PushInst(List(lr))
        , new MovInst(r1, r0)
        , new LdrInst(null, LabelAddress(text))
        )) 
      CodeGenerator.currInstBlock.addInst(printEnd())
      CodeGenerator.controlFlowFuncs.addOne(PRINT_INT_LABEL, printIntFunc)
    }

    def printPtr(op: Operand): Unit = {
      /* caller instruction */
      CodeGenerator.currInstBlock.addInst(
        List(
          new MovInst(r0, op),
          new BranchLinkInst(PRINT_PTR_LABEL)
          )
      )

      /* callee instruction */
      val printPtrFunc = new FuncBlock()
      val text = printPtrFunc.directive.addTextLabelToData("%p", PRINT_PTR_LABEL)
      printPtrFunc.body.addInst(
        List(
          new PushInst(List(lr))
        , new MovInst(r1, r0)
        , new LdrInst(null, LabelAddress(text))
        )) 
      CodeGenerator.currInstBlock.addInst(printEnd())
      CodeGenerator.controlFlowFuncs.addOne(PRINT_PTR_LABEL, printPtrFunc)
    }

    def printString(op: Operand): Unit = {
      CodeGenerator.currInstBlock.addInst(
        List(
          new LdrInst(r0, op)
        , new BranchLinkInst(PRINT_STR_LABEL)
        ))

      /* callee instruction */
      val printStringFunc = new FuncBlock()
      val text = printStringFunc.directive.addTextLabelToData("%s", PRINT_STR_LABEL)

      printStringFunc.body.addInst(
        List(
          new PushInst(List(lr))
        , new MovInst(r2, r0)
        , new LdrInst(r1, new Offset(r0, arrLenOffset))
        , new LdrInst(null, new LabelAddress(text))
        )
      )
      CodeGenerator.currInstBlock.addInst(printEnd())     
      CodeGenerator.controlFlowFuncs.addOne(PRINT_STR_LABEL, printStringFunc)
    }

    def printChar(op: Operand): Unit = {
      /* caller instruction */
      CodeGenerator.currInstBlock.addInst(
        List(
          new MovInst(r0, op),
          new BranchLinkInst(PRINT_CHAR_LABEL)
          )
      )

      /* callee instruction */
      val printCharFunc = new FuncBlock()
      val text = printCharFunc.directive.addTextLabelToData("%c", PRINT_CHAR_LABEL)
      printCharFunc.body.addInst(
        List(
          new PushInst(List(lr))
        , new MovInst(r1, r0)
        , new LdrInst(null, LabelAddress(text))
        )) 
      CodeGenerator.currInstBlock.addInst(printEnd())
      CodeGenerator.controlFlowFuncs.addOne(PRINT_CHAR_LABEL, printCharFunc)
    }

    def printBool(op: Operand): Unit = {
      /* caller instruction */
      CodeGenerator.currInstBlock.addInst(
        List(
          new MovInst(r0, op),
          new BranchLinkInst(PRINT_BOOL_LABEL)
          )
      )

      /* callee instruction */
      val printBoolFunc = new FuncBlock()
      val falseTxt = printBoolFunc.directive.addTextLabelToData("%false", PRINT_BOOL_LABEL)
      val trueTxt = printBoolFunc.directive.addTextLabelToData("%true", PRINT_BOOL_LABEL)
      val text = printBoolFunc.directive.addTextLabelToData("%s", PRINT_BOOL_LABEL)

      val ifBlock = IfBlock()
      val ifTrue = ifBlock.nextT
      val ifFalse = ifBlock.nextF
      val next = ifBlock.next

      printBoolFunc.body.addInst(
        List(
            new PushInst(List(lr))
          , new CmpInst(r0, new ImmVal(0, new IntIdentifier()))
          , new BranchNumCondInst("NE", ifFalse.num)
        )
      )
      /* print true */
      CodeGenerator.currInstBlock = ifTrue
      CodeGenerator.currInstBlock.addInst(
        new LdrInst(r2, new LabelAddress(trueTxt))
      )
      /* print false */
      CodeGenerator.currInstBlock = ifFalse
      CodeGenerator.currInstBlock.addInst(
          new LdrInst(r2, new LabelAddress(falseTxt))
      )
      /* next block */
      CodeGenerator.currInstBlock = next
      CodeGenerator.currInstBlock.addInst(
        List(
            new LdrInst(r1, new Offset(r2, arrLenOffset))
          , new LdrInst(r0, new LabelAddress(text))
        )
      )
      CodeGenerator.currInstBlock.addInst(printEnd())

      CodeGenerator.controlFlowFuncs.addOne(PRINT_BOOL_LABEL, printBoolFunc)
    }

    def println(): Unit = {
      val printlnFunc = new FuncBlock()
      val text = printlnFunc.directive.addTextLabelToData("", PRINTLN_LABEL)
      printlnFunc.body.addInst(
        List(
          new PushInst(List(lr))
        , new LdrInst(null, LabelAddress(text))
        , new BranchLinkInst("puts")
        )) 
      CodeGenerator.currInstBlock.addInst(printEnd())
      CodeGenerator.controlFlowFuncs.addOne(PRINTLN_LABEL, printlnFunc)

    }

    def readChar(op: Operand): Unit = {
      /* caller instruction */
      op match {
        case op: Register => {
            CodeGenerator.currInstBlock.addInst(List(
            new MovInst(r0, op)
          , new BranchLinkInst(READ_CHAR_LABEL)
          , new MovInst(op, r0)
          ))
        }
        case _ => throw new IllegalArgumentException("read op is not register")
      }
      /* callee instruction */
      val readCharFunc = new FuncBlock()     
      readCharFunc.body.addInst(readFunc(READ_CHAR_LABEL, "%c", readCharFunc))
      CodeGenerator.controlFlowFuncs.addOne(READ_CHAR_LABEL, readCharFunc)    
    }

    def readInt(op: Operand): Unit = {
      /* caller instruction */
      op match {
        case op: Register => {
            CodeGenerator.currInstBlock.addInst(List(
            new MovInst(r0, op)
          , new BranchLinkInst(READ_INT_LABEL)
          , new MovInst(op, r0)
          ))
        }
        case _ => throw new IllegalArgumentException("read op is not register")
      }
      /* callee instruction */
      val readIntFunc = new FuncBlock()
      readIntFunc.body.addInst(readFunc(READ_CHAR_LABEL, "%d", readIntFunc))
      CodeGenerator.controlFlowFuncs.addOne(READ_INT_LABEL, readIntFunc)    
    }
}
