package wacc

import parsley.{Parsley, Success, Failure}
import Parser.topLevel

import scala.io.Source
import wacc.Constants._
import java.io._ //{BufferedWriter, File, FileWriter}

object Main {
    def main(args: Array[String]): Unit = {
        println("Hello WACC_45!")

        val fileContents = Source.fromFile(args(0)).getLines().mkString("\n")
        println("File contents:")
        println(fileContents)
        
        topLevel.parse(fileContents) match {
            case Success(x) => {
                println(s"AST = $x")
                SemanticChecker.check(x)
                if (!Error.exitWithSemanticErr()) {
                    println("No semantic error")
                    
                    /* AST --> IR1 */
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
                //Disabling exit code for sbt debug session
                sys.exit(100)
            }
        }
    }


    object WriteToFile {

        def fileName(filePath: String) : String = {
            filePath.split("/").last.dropRight(5) + ".s"
        }

        def write(filename: String): Unit = {        

            /* IR1 --> IR2 */
            AssignRegister.assignCFG(CodeGenerator.controlFlowFuncs)

            val pw = new PrintWriter(new File(filename))
            /* global main */
            for ((name, funcBlock) <- CodeGenerator.controlFlowFuncs) {
                Printer.printBlock(funcBlock)
            }

            addStandardFuncs()

            for (line <- Printer.output) {
                pw.println(line)
            }

            pw.close()              
        }

        def addStandardFuncs(): Unit = {
            for (i <- 0 until StandardFuncs.usedFuncs.length) {
                if (StandardFuncs.usedFuncs(i)) {
                    Printer.printBlock(StandardFuncs.getFunction(StdFuncsEnum(i)))
                }
            }
            // if (ArrayStore.getUsed) {
            //     Printer.printBlock(ArrayStore.getFunc)
            // }
            // if (ArrayStoreB.getUsed) {
            //     Printer.printBlock(ArrayStoreB.getFunc)
            // }
            // if (ArrayLoad.getUsed) {
            //     Printer.printBlock(ArrayLoad.getFunc)
            // }
            // if (ArrayLoadB.getUsed) {
            //     Printer.printBlock(ArrayLoadB.getFunc)
            // }
            // if (FreePair.getUsed) {
            //     Printer.printBlock(FreePair.getFunc)
            // }
            // if (ZeroDivision.getUsed) {
            //     Printer.printBlock(ZeroDivision.getFunc)
            // }
            // if (NullPointer.getUsed) {
            //     Printer.printBlock(NullPointer.getFunc)
            // }
            // if (Overflow.getUsed) {
            //     Printer.printBlock(Overflow.getFunc)
            // }
            // if (BoundsCheck.getUsed) {
            //     Printer.printBlock(BoundsCheck.getFunc)
            // }
        }
    }
}