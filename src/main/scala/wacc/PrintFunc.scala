package wacc

class IOFunc() {
  /**
    *  _printi: print integer
    *  _prints: print string
    *  _printc: print char
    *  _printb: print boolean
    *  _println: bl _print<type> ,then  tprints new line character
    */
    val PRINT_INT_LABEL = "_printi"
    val PRINT_STR_LABEL = "_prints"
    val PRINT_CHAR_LABEL = "_printc"
    val PRINT_BOOL_LABEL = "_printb"
    val PRINTLN_LABEL = "_println"

    val READ_INT_LABEL = "_readi"
    val READ_CHAR_LABEL = "_readc"

    val printEnd = List (
      new BranchLinkInst("printf"),
      new MovInst(null, null),
      new BranchLinkInst("fflush"),
      new PopInst(null))

    def addPrintLabelToData(text: String, printType: String, funckBlock: FuncBlock) : String = {
      val content = new StringBuilder()
      content ++= s"  .word ${text.length()}\n" +
                  s".L.${printType}_str${funckBlock.labels.labelCount}:\n" +
                  s"  .asciz \"${text}\"\n" +
                  ".text\n"
      funckBlock.labels.addToPrintDataSubsection(content.toString())
      // returns label string for text added to .data directive
      s".L.${printType}_str${funckBlock.labels.labelCount}"
    }

    def printInt(op: Operand): Unit = {
      val calleeReg = new TempRegister()
      val callerReg = new TempRegister()
      /* caller instruction */
      val intToPrint = new MovInst(calleeReg, op)
      val instr2 = new MovInst(callerReg, calleeReg)
      val instr3 = new BranchLinkInst(PRINT_INT_LABEL)
      CodeGenerator.currInstBlock.addInst(List(intToPrint, instr2, instr3))

      /* callee instruction */
      val printIntFunc = new FuncBlock()
      addPrintLabelToData("%d", PRINT_INT_LABEL, printIntFunc)

      val pinstr1 = new PushInst(List())
      val pinstr2 = new MovInst(null, null)
      val pinstr3 = new LdrInst(null, LabelAddress(s"=${PRINT_INT_LABEL}"))

      printIntFunc.body.addInst(List(pinstr1, pinstr2, pinstr3)) 
      CodeGenerator.currInstBlock.addInst(printEnd)
      CodeGenerator.controlFlowFuncs.addOne(printIntFunc)
    }

    def printString(op: Operand): Unit = {
      val calleeReg = new TempRegister()
      val callerReg = new TempRegister()
      /* caller instruction */
      val loadTextToFunc = new LdrInst(null, null)
      val push = new PushInst(null)
      val pop = new PopInst(null)
      val strToPrint = new MovInst(calleeReg, op)
      val instr2 = new MovInst(callerReg, calleeReg)
      val instr3 = new BranchLinkInst(PRINT_STR_LABEL)
      CodeGenerator.currInstBlock.addInst(List(loadTextToFunc, push, pop, strToPrint, instr2, instr3))

      /* callee instruction */
      val printStringFunc = new FuncBlock()
      addPrintLabelToData("%s", PRINT_STR_LABEL, printStringFunc)

      val pinstr1 = new PushInst(List())
      val pinstr2 = new MovInst(null, null)
      val loadTextContent = new LdrInst(null, null)
      val pinstr3 = new LdrInst(null, LabelAddress(s"=${PRINT_STR_LABEL}"))
      printStringFunc.body.addInst(
        List(pinstr1, pinstr2, loadTextContent, pinstr3))
      CodeGenerator.currInstBlock.addInst(printEnd)     
      CodeGenerator.controlFlowFuncs.addOne(printStringFunc)
    }

    def printChar(op: Operand): Unit = {
      val calleeReg = new TempRegister()
      val callerReg = new TempRegister()
      /* caller instruction */
      val charToPrint = new MovInst(calleeReg, op)
      val instr2 = new MovInst(callerReg, calleeReg)
      val instr3 = new BranchLinkInst(PRINT_CHAR_LABEL)
      CodeGenerator.currInstBlock.addInst(List(charToPrint, instr2, instr3))

      /* callee instruction */
      val printCharFunc = new FuncBlock()
      addPrintLabelToData("%c", PRINT_CHAR_LABEL, printCharFunc)

      val pinstr1 = new PushInst(List())
      val pinstr2 = new MovInst(null, null)
      val pinstr3 = new LdrInst(null, LabelAddress(s"=${PRINT_CHAR_LABEL}"))

      printCharFunc.body.addInst(
        List(pinstr1, pinstr2, pinstr3)) 
      CodeGenerator.currInstBlock.addInst(printEnd)     
      CodeGenerator.controlFlowFuncs.addOne(printCharFunc)
    }

    def printBool(op: Operand): Unit = {
      val calleeReg = new TempRegister()
      val callerReg = new TempRegister()
      /* caller instruction */
      val boolToPrint = new MovInst(calleeReg, op)
      val instr2 = new MovInst(callerReg, calleeReg)
      val instr3 = new BranchLinkInst(PRINT_INT_LABEL)
      CodeGenerator.currInstBlock.addInst(List(boolToPrint, instr2, instr3))

      /* callee instruction */
      val printBoolFunc = new FuncBlock()
      addPrintLabelToData("%false", PRINT_CHAR_LABEL, printBoolFunc)
      addPrintLabelToData("%true", PRINT_CHAR_LABEL, printBoolFunc)
      addPrintLabelToData("%s", PRINT_CHAR_LABEL, printBoolFunc)

      val pinstr1 = new PushInst(List())
      val pinstr2 = new CmpInst(null, ImmVal(0, new BoolIdentifier))

      //val trueOrFalse = new IfBlock() ????? add it tp control flow graph??????
      printBoolFunc.body.addInst(
        List(pinstr1, pinstr2))
      CodeGenerator.currInstBlock.addInst(printEnd)    
      CodeGenerator.controlFlowFuncs.addOne(printBoolFunc)
    }

    def readChar(op: Operand): Unit = {
      val calleeReg = new TempRegister()
      /* caller instruction */
      val charToRead = new MovInst(calleeReg, op)
      val instr2 = new BranchLinkInst(READ_CHAR_LABEL)
      CodeGenerator.currInstBlock.addInst(List(charToRead, instr2))

      /* callee instruction */
      val readCharFunc = new FuncBlock()
      addPrintLabelToData("%c", READ_CHAR_LABEL, readCharFunc)
      val list = List(
        new StrInst(new TempRegister(), null),
        new MovInst(new TempRegister(), null),
        new LdrInst(TempRegister(), new LabelAddress("=.L._readc_str0")),
        new BranchInst("scanf"),
        new AddInst(null, null, null),
        new PopInst(null)
      )
      readCharFunc.body.addInst(list)
      CodeGenerator.controlFlowFuncs.addOne(readCharFunc)    
    }

    def readInt(op: Operand): Unit = {
      val calleeReg = new TempRegister()
      /* caller instruction */
      val intToRead = new MovInst(calleeReg, op)
      val instr2 = new BranchLinkInst(READ_INT_LABEL)
      CodeGenerator.currInstBlock.addInst(List(intToRead, instr2))

      /* callee instruction */
      val readIntFunc = new FuncBlock()
      addPrintLabelToData("%d", READ_INT_LABEL, readIntFunc)
      val list = List(
        new StrInst(new TempRegister(), null),
        new MovInst(new TempRegister(), null),
        new LdrInst(TempRegister(), new LabelAddress("=.L._readi_str0")),
        new BranchInst("scanf"),
        new AddInst(null, null, null),
        new PopInst(null)
      )
      readIntFunc.body.addInst(list)
      CodeGenerator.controlFlowFuncs.addOne(readIntFunc)    
    }
}
