package wacc

import scala.collection.mutable

object Error {
    private val errors = mutable.ListBuffer[String]()

    def addErr(s: String) = {
        errors += s;
    }

    def printErr() = {
        errors.foreach(println)
    }
    
}

// sealed trait ErrorLines
// case class VanillaError(unexpected: Option[ErrorItem], expected: Set[ErrorItem], reasons: Set[String], line: LineInfo)

// sealed trait ErrorItem
// case class Raw(item: String) extends ErrorItem
// case class Named(item: String) extends ErrorItem
// case object EndOfInput extends ErrorItem

// case class LineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int)
