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
                    //CodeGenerator.translateAST(x)
                    ASTtoCode.setNode(x)
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

    object ASTtoCode {
        var astNode: Option[ProgramNode] = None
        def setNode(node: ProgramNode):Unit = {
            astNode = Some(node)
        }
        def getNode(): ProgramNode = {
            astNode.get
        }

    }

    object WriteToFile {

        def fileName(filePath: String) : String = {
            filePath.split("/").last.dropRight(5) + ".s"
        }

        def write(filename: String): Unit = {
            /* add main func to func list and set main as global main*/
            val mainFunc = new FuncBlock()
            mainFunc.directive.setGlobalMain()
            mainFunc.name = "main"
            CodeGenerator.controlFlowFuncs.addOne("main", mainFunc)
            CodeGenerator.mainFunc = mainFunc
            CodeGenerator.controlFlowGraph = mainFunc
            
            CodeGenerator.translateAST(ASTtoCode.getNode())
            // does not return
            
            val pw = new PrintWriter(new File(filename))
            /* global main */
            for ((name, funcBlock) <- CodeGenerator.controlFlowFuncs) {
                Printer.printBlock(funcBlock)
            }
            var i = 0
            for (line <- Printer.output) {
                pw.println(line)
                i += 1
            }
            val begin =
                // ".data\n" +
                // ".text\n" +
                // ".global main\n" +
                // "main:\n" +
                "  push {fp, lr}\n" +
                "  push {r8, r10, r12}\n" +
                "  mov fp, sp\n"
            pw.print(begin)
            val end =
                "  mov r0, #0\n" +
                "  pop {r8, r10, r12}\n" +
                "  pop {fp, pc}\n"
            pw.print(end)

            /* funcs */
            for (func <- CodeGenerator.controlFlowFuncs) {
                //pw.println(func)
            }

            pw.close()              
        }
    }
}