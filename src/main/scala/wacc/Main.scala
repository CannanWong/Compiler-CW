package wacc

import parsley.{Parsley, Success, Failure}
import Parser.topLevel

import scala.io.Source
import better.files._
import File._
import java.io.{File => JFile}

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
                    translate(x)
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

    def translate(ast: ProgramNode): Unit = {
        val f = File("/home/cannan/WACC_45/skip.s")
        val str = (".data\n" +
          ".text\n" +
          ".global main\n" +
          "main:\n" +
          "  push {fp, lr}\n" +
          "  push {r8, r10, r12}\n" +
          "  mov fp, sp\n" +
          "  mov r0, #0\n" +
          "  pop {r8, r10, r12}\n" +
          "  pop {fp, pc}\n")
        f.overwrite(str)
    }
}

