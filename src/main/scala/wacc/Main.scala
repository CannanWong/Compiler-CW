package wacc

import parsley.{Parsley, Success, Failure}
import Parser.topLevel

import scala.io.Source

object Main {
    def main(args: Array[String]): Unit = {
        println("Hello WACC_45!")

        val fileContents = Source.fromFile(args(0)).getLines().mkString("\n")
        println("File contents:")
        println(fileContents)
        
        topLevel.parse(fileContents) match {
            case Success(x) => {
                println(s"AST = $x")
                val result = SemanticChecker.check(x)
                if (result == "") {
                    println("No semantic error")
                }
                else {
                    println("#semantic_error#\n" + result)
                    sys.exit(200)
                }
            }
            case Failure(msg) => {
                println("#syntax_error#\n" + msg)
                //Disabling exit code for sbt debug session
                sys.exit(100)
            }
        }
    }
}

