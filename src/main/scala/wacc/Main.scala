package wacc

import parsley.{Success, Failure}
import Parser.topLevel

import scala.io.Source
import wacc.Constants._
import wacc.IOFunc.PRINT_STR_LABEL
import java.io._ 

object Main {
    def main(args: Array[String]): Unit = {

        val fileContents = Source.fromFile(args(0)).getLines().mkString("\n")
        
        topLevel.parse(fileContents) match {
            // Parsing successful
            case Success(x) => {
                SemanticChecker.check(x)
                if (!Error.exitWithSemanticErr()) {
                    println("No syntax or semantic error")

                    /* AST --> IR1: Control Flow Graph */
                    CodeGenerator.translateAST(x)

                    val filename = WriteToFile.fileName(args(0))
                    WriteToFile.write(filename)
                }
                else {
                    println("#semantic_error#\n")
                    Error.printErr()
                    sys.exit(200)
                }
            }
            case Failure(msg) => {
                Error.addErr(msg)
                println("#syntax_error#\n" +
                  "Syntax error detected during parsing, Exit code: 100.\n" +
                  "Error at")
                Error.printErr()
                sys.exit(100)
            }
        }
    }


    object WriteToFile {

        def fileName(filePath: String) : String = {
            filePath.split("/").last.dropRight(5) + ".s"
        }

        def write(filename: String): Unit = {        

            /* IR1 --> IR2: Assign registers */
            AssignRegister.assignCFG(CodeGenerator.controlFlowFuncs)

            val file = new File(filename)
            val bw = new BufferedWriter(new FileWriter(file))
            /* global main */
            for ((name, funcBlock) <- CodeGenerator.controlFlowFuncs) {
                Printer.printBlock(funcBlock)
            }

            addStandardFuncs()
            if (!CodeGenerator.controlFlowFuncs.contains(PRINT_STR_LABEL)) {
                val printStringFunc = FuncBlock()
                printStringFunc.name = PRINT_STR_LABEL

                val text = printStringFunc.directive.addTextLabelToData("%.*s", PRINT_STR_LABEL)

                printStringFunc.body.addInst(
                    PushInst(lr),
                    MovInst(r2, r0),
                    LdrInst(r1, ImmOffset(r0, INT_SIZE)),
                    LdrInst(r0, LabelAddress(text))
                )
                printStringFunc.body.addInst(IOFunc.printEnd():_*)
                Printer.printBlock(printStringFunc)
            }

            try {
                Printer.output.foreach { s =>
                bw.write(s)
                bw.newLine()
                }
            } finally {
                bw.close()
            }             
        }

        def addStandardFuncs(): Unit = {
            for (i <- 0 until StandardFuncs.usedFuncs.length) {
                if (StandardFuncs.usedFuncs(i)) {
                    Printer.printBlock(StandardFuncs.getFunction(StdFuncsEnum(i)))
                }
            }
        }
    }
}