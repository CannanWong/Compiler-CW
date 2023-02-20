package wacc

import parsley.{Parsley, Success, Failure}
import Parser.topLevel

import scala.io.Source
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
        val generatedCode: List[String] = List.empty

        def fileName(filePath: String) : String = {
            filePath.split("/").last.dropRight(5) + ".s"
        }

        def write(filename: String): Unit = {
            val pw = new PrintWriter(new File(filename))
            val default = "abc"
                // ".data\n" +
                // ".text\n" +
                // ".global main\n" +
                // "main:\n" +
                // "  push {fp, lr}\n" +
                // "  push {r8, r10, r12}\n" +
                // "  mov fp, sp\n" +
                // "  mov r0, #0\n" +
                // "  pop {r8, r10, r12}\n" +
                // "  pop {fp, pc}\n"
            pw.write(default)
            // for (instr <- generatedCode) {
            //     bw.write(instr)
            //     bw.newLine()
            // }
            pw.close()              
        }
    }
}

