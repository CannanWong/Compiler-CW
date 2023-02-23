package wacc

class PrintFunc() {
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
      val calleeReg = null
      val callerReg = null
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

      CodeGenerator.currInstBlock.addInst(List(pinstr1, pinstr2, pinstr3)) 
      CodeGenerator.currInstBlock.addInst(printEnd)
      CodeGenerator.controlFlowFuncs.addOne(printIntFunc)
    }

    def printString(op: Operand): Unit = {
      val calleeReg = null
      val callerReg = null
      /* caller instruction */
      val loadTextToFunc = new LdrInst(null, null)
      val push = new PushInst(null)
      val pop = new PopInst(null)
      val strToPrint = new MovInst(calleeReg, op)
      val instr2 = new MovInst(callerReg, calleeReg)
      val instr3 = new BranchLinkInst(PRINT_STR_LABEL)
      CodeGenerator.currInstBlock.addInst(List(loadTextToFunc, push, pop, strToPrint, instr2, instr3))

      /* callee instruction */
      val printIntFunc = new FuncBlock()
      addPrintLabelToData("%s", PRINT_STR_LABEL, printIntFunc)

      val pinstr1 = new PushInst(List())
      val pinstr2 = new MovInst(null, null)
      val loadTextContent = new LdrInst(null, null)
      val pinstr3 = new LdrInst(null, LabelAddress(s"=${PRINT_STR_LABEL}"))
      CodeGenerator.currInstBlock.addInst(
        List(pinstr1, pinstr2, loadTextContent, pinstr3))
      CodeGenerator.currInstBlock.addInst(printEnd)     
      CodeGenerator.controlFlowFuncs.addOne(printIntFunc)
    }

    def printChar(op: Operand): Unit = {
      val calleeReg = null
      val callerReg = null
      /* caller instruction */
      val charToPrint = new MovInst(calleeReg, op)
      val instr2 = new MovInst(callerReg, calleeReg)
      val instr3 = new BranchLinkInst(PRINT_CHAR_LABEL)
      CodeGenerator.currInstBlock.addInst(List(charToPrint, instr2, instr3))

      /* callee instruction */
      val printIntFunc = new FuncBlock()
      addPrintLabelToData("%c", PRINT_CHAR_LABEL, printIntFunc)

      val pinstr1 = new PushInst(List())
      val pinstr2 = new MovInst(null, null)
      val pinstr3 = new LdrInst(null, LabelAddress(s"=${PRINT_CHAR_LABEL}"))

      CodeGenerator.currInstBlock.addInst(
        List(pinstr1, pinstr2, pinstr3)) 
      CodeGenerator.currInstBlock.addInst(printEnd)     
      CodeGenerator.controlFlowFuncs.addOne(printIntFunc)
    }

    def printBool(op: Operand): Unit = {
      val calleeReg = null
      val callerReg = null
      /* caller instruction */
      val boolToPrint = new MovInst(calleeReg, op)
      val instr2 = new MovInst(callerReg, calleeReg)
      val instr3 = new BranchLinkInst(PRINT_INT_LABEL)
      CodeGenerator.currInstBlock.addInst(List(boolToPrint, instr2, instr3))

      /* callee instruction */
      val printIntFunc = new FuncBlock()
      addPrintLabelToData("%false", PRINT_CHAR_LABEL, printIntFunc)
      addPrintLabelToData("%true", PRINT_CHAR_LABEL, printIntFunc)
      addPrintLabelToData("%s", PRINT_CHAR_LABEL, printIntFunc)

      val pinstr1 = new PushInst(List())
      val pinstr2 = new CmpInst(null, ImmVal(0, new BoolIdentifier))

      //val trueOrFalse = new IfBlock() ????? add it tp control flow graph??????
      CodeGenerator.currInstBlock.addInst(
        List(pinstr1, pinstr2))
      CodeGenerator.currInstBlock.addInst(printEnd)    
      CodeGenerator.controlFlowFuncs.addOne(printIntFunc)

    }
}
