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

        // lazy val integer = digit.foldLeft1[BigInt](0)((n, d) => n * 10 + d.asDigit)

        // val add = (x: BigInt, y: BigInt) => x + y
        // val sub = (x: BigInt, y: BigInt) => x - y

        // lazy val expr: Parsley[BigInt] =
        //     chain.left1[BigInt](
        //         ('(' ~> expr <~ ')') <|> integer,
        //         ('+' #> add) <|> ('-' #> sub)
        //     )

        topLevel.parse(fileContents) match {
            case Success(x) => println(s"AST = $x")
            case Failure(msg) => {
                println("#syntax_error#\n" + msg)
                //Disabling exit code for sbt debug session
                sys.exit(100)
            }
        }
    }
}

