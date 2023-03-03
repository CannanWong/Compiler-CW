package wacc

import wacc.Constants._

object IOFunc {
  	/* text label for read and print functions */
    val PRINT_INT_LABEL = "_printi"
    val PRINT_STR_LABEL = "_prints"
    val PRINT_CHAR_LABEL = "_printc"
    val PRINT_BOOL_LABEL = "_printb"
    val PRINT_PTR_LABEL = "_printp"
    val PRINTLN_LABEL = "_println"

    val READ_INT_LABEL = "_readi"
    val READ_CHAR_LABEL = "_readc"

  	/* instructions for read int function */
    def printEnd(): List[Instruction] = {
      	List (
        	BranchLinkInst("printf"),
        	MovInst(r0, ImmVal(0)),
        	BranchLinkInst("fflush"),
        	PopInst(pc)
      	)
    }

    /* instructions for read int function */
   		def readFunc(label: String, printType: String, funcBlock: FuncBlock): List[Instruction] = {
      	val labelStr = funcBlock.directive.addTextLabelToData(printType, label)
      	List(
        	PushInst(lr),
        	StrChgInst(r0, ImmOffset(sp, INT_SIZE)),
			MovInst(r1, sp),
			LdrInst(r0, LabelAddress(labelStr)),
			BranchLinkInst("scanf"),
			LdrInst(r0, ImmOffset(sp, 0)),
			AddInst(sp, sp, ImmVal(data_offset(INT_SIZE))),
			PopInst(pc)
		)
    }

    /* instructions for read char function */
    def readCFunc(label: String, printType: String, funcBlock: FuncBlock): List[Instruction] = {
		val labelStr = funcBlock.directive.addTextLabelToData(printType, label)
		List(
			PushInst(lr),
			StrbChgInst(r0, ImmOffset(sp, BYTE_SIZE)),
			MovInst(r1, sp),
			LdrInst(r0, LabelAddress(labelStr)),
			BranchLinkInst("scanf"),
			LdrsbInst(r0, ImmOffset(sp, 0)),
			AddInst(sp, sp, ImmVal(data_offset(BYTE_SIZE))),
			PopInst(pc)
		)
    }

    def printInt(op: Operand): Unit = {
		/* caller instruction */
		CodeGenerator.currInstBlock.addInst(
			MovInst(r0, op),
			BranchLinkInst(PRINT_INT_LABEL)
      	)

		/* callee instruction */
		val printIntFunc = FuncBlock()
		printIntFunc.name = PRINT_INT_LABEL

		val text = printIntFunc.directive.addTextLabelToData("%d", PRINT_INT_LABEL)
		printIntFunc.body.addInst(
			PushInst(lr),
			MovInst(r1, r0),
			LdrInst(r0, LabelAddress(text))
		)
		printIntFunc.body.addInst(printEnd():_*)
		CodeGenerator.controlFlowFuncs.addOne(PRINT_INT_LABEL, printIntFunc)
		}

		def printPtr(op: Operand): Unit = {
		/* caller instruction */
		CodeGenerator.currInstBlock.addInst(
			MovInst(r0, op),
			BranchLinkInst(PRINT_PTR_LABEL)
		)

		/* callee instruction */
		val printPtrFunc = FuncBlock()
		printPtrFunc.name = PRINT_PTR_LABEL

		val text = printPtrFunc.directive.addTextLabelToData("%p", PRINT_PTR_LABEL)
		printPtrFunc.body.addInst(
			PushInst(lr),
			MovInst(r1, r0),
			LdrInst(r0, LabelAddress(text))
		)
		printPtrFunc.body.addInst(printEnd():_*)
		CodeGenerator.controlFlowFuncs.addOne(PRINT_PTR_LABEL, printPtrFunc)
    }

    def printString(op: Operand): FuncBlock = {
		CodeGenerator.currInstBlock.addInst(
			MovInst(r0, op),
			BranchLinkInst(PRINT_STR_LABEL)
		)

		/* callee instruction */
		val printStringFunc = FuncBlock()
		printStringFunc.name = PRINT_STR_LABEL

		val text = printStringFunc.directive.addTextLabelToData("%.*s", PRINT_STR_LABEL)

		printStringFunc.body.addInst(
			PushInst(lr),
			MovInst(r2, r0),
			LdrInst(r1, ImmOffset(r0, INT_SIZE)),
			LdrInst(r0, LabelAddress(text))
		)
		printStringFunc.body.addInst(printEnd():_*)
		CodeGenerator.controlFlowFuncs.addOne(PRINT_STR_LABEL, printStringFunc)

		printStringFunc
    }

    def printChar(op: Operand): Unit = {
		/* caller instruction */
		CodeGenerator.currInstBlock.addInst(
			MovInst(r0, op),
			BranchLinkInst(PRINT_CHAR_LABEL)
		)

		/* callee instruction */
		val printCharFunc = FuncBlock()
		printCharFunc.name = PRINT_CHAR_LABEL
		val text = printCharFunc.directive.addTextLabelToData("%c", PRINT_CHAR_LABEL)
		printCharFunc.body.addInst(
			PushInst(lr),
			MovInst(r1, r0),
			LdrInst(r0, LabelAddress(text))
		)
		printCharFunc.body.addInst(printEnd():_*)
		CodeGenerator.controlFlowFuncs.addOne(PRINT_CHAR_LABEL, printCharFunc)
    }

    def printBool(op: Operand): Unit = {
		/* caller instruction */
		CodeGenerator.currInstBlock.addInst(
			MovInst(r0, op),
			BranchLinkInst(PRINT_BOOL_LABEL)
		)

		/* callee instruction */
		val printBoolFunc = FuncBlock()
		printBoolFunc.name = PRINT_BOOL_LABEL
		val falseTxt = printBoolFunc.directive.addTextLabelToData("false", PRINT_BOOL_LABEL)
		val trueTxt = printBoolFunc.directive.addTextLabelToData("true", PRINT_BOOL_LABEL)
		val text = printBoolFunc.directive.addTextLabelToData("%.*s", PRINT_BOOL_LABEL)

		val ifBlock = IfBlock()
		val ifTrue = ifBlock.nextT
		val ifFalse = ifBlock.nextF
		val next = ifBlock.next

		printBoolFunc.currBlock.next = ifBlock

		printBoolFunc.body.addInst(
			PushInst(lr),
			CmpInst(r0, ImmVal(0)),
			BranchNumCondInst(NOT_EQUAL, ifFalse.num)
		)
		ifTrue.addInst(
			LdrInst(r2, LabelAddress(falseTxt)),
			BranchNumInst(next.num)
		)
		ifFalse.addInst(LdrInst(r2, LabelAddress(trueTxt)))
		next.addInst(
			LdrInst(r1, ImmOffset(r2, INT_SIZE)),
			LdrInst(r0, LabelAddress(text))
		)
		next.addInst(printEnd():_*)

		CodeGenerator.controlFlowFuncs.addOne(PRINT_BOOL_LABEL, printBoolFunc)
    }

    def println(): Unit = {
		CodeGenerator.currInstBlock.addInst(
			BranchLinkInst(PRINTLN_LABEL)
		)
		/* callee instruction */
		val printlnFunc = FuncBlock()
		printlnFunc.name = PRINTLN_LABEL

		val text = printlnFunc.directive.addTextLabelToData("", PRINTLN_LABEL)
		printlnFunc.body.addInst(
			PushInst(lr),
			LdrInst(r0, LabelAddress(text)),
			BranchLinkInst("puts"),
			MovInst(r0, ImmVal(0)),
			BranchLinkInst("fflush"),
			PopInst(pc)
		)
		CodeGenerator.controlFlowFuncs.addOne(PRINTLN_LABEL, printlnFunc)
    }

    def readChar(op: Operand): Unit = {
		/* caller instruction */
		op match {
			case op: Register => {
			CodeGenerator.currInstBlock.addInst(
				PushInst(r0, r1, r2, r3),
				MovInst(r0, op),
				BranchLinkInst(READ_CHAR_LABEL),
				MovInst(op, r0),
				PopInst(r0, r1, r2, r3)
			)
			}
			case _ => throw new IllegalArgumentException("read op is not register")
		}
		/* callee instruction */
		val readCharFunc = FuncBlock()
		readCharFunc.name = READ_CHAR_LABEL

		readCharFunc.body.addInst(readCFunc(READ_CHAR_LABEL, " %c", readCharFunc))

		CodeGenerator.controlFlowFuncs.addOne(READ_CHAR_LABEL, readCharFunc)    
    }

    def readInt(op: Operand): Unit = {
		/* caller instruction */
		op match {
			case op: Register => {
			CodeGenerator.currInstBlock.addInst(
				PushInst(r0, r1, r2, r3),
				MovInst(r0, op),
				BranchLinkInst(READ_INT_LABEL),
				MovInst(op, r0),
				PopInst(r0, r1, r2, r3)
			)
			}
			case _ => throw new IllegalArgumentException("read op is not register")
		}
		/* callee instruction */
		val readIntFunc = FuncBlock()
		readIntFunc.name = READ_INT_LABEL

		readIntFunc.body.addInst(readFunc(READ_INT_LABEL, "%d", readIntFunc))

		CodeGenerator.controlFlowFuncs.addOne(READ_INT_LABEL, readIntFunc)    
    }
}
