package wacc

class Print() {
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

    def addPrintLabelToData(text: String, printType: String, funckBlock: FuncBlock) : String = {
      val content = new StringBuilder()
      content ++= s"  .word ${text.length()}\n" +
                  s".L.${printType}_str${funckBlock.labels.labelCount}\n" +
                  s"  .asciz \"${text}\"\n" +
                  ".text\n"
      funckBlock.labels.addToPrintDataSubsection(content.toString())
      // returns label string for text added to .data directive
      s".L.${printType}_str${funckBlock.labels.labelCount}"
    }

    def printInt(op: Operand): Unit = {
      /* caller instruction */

      val instr1 = new MovInst(null, null)
      val instr3 = new MovInst(null, null)
      val instr2 = new BranchLinkInst(PRINT_INT_LABEL)
      CodeGenerator.currInstBlock.addInst(List(instr1, instr2, instr3))

      /* callee instruction */
      val printIntFunc = new FuncBlock()
      addPrintLabelToData("%d", PRINT_INT_LABEL, printIntFunc)

      val pinstr1 = new PushInst(List())
      val pinstr2 = new MovInst(null, null)
      val pinstr3 = new LdrInst(null, null)
      val pinstr4 = new BranchLinkInst("printf")
      val pinstr5 = new MovInst(null, null)
      val pinstr6 = new BranchLinkInst("fflush")
      val pinstr7 = new PopInst(null)
      CodeGenerator.currInstBlock.addInst(
        List(pinstr1, pinstr2, pinstr3, pinstr4, pinstr5, pinstr6, pinstr7))      
      CodeGenerator.controlFlowFuncs.addOne(printIntFunc)
    }

    def printString(): Unit = {

    }

    def printChar(): Unit = {

    }

    def printBool(): Unit = {

    }
  
}