package wacc

import scala.collection.mutable

object Error {
    private val errors = mutable.ListBuffer[String]()
    private var semanticErrExist = false

    def addErr(s: String) = {
        errors += s
    }

    def addSemErr(s: String) = {
        semanticErrExist = true
        errors += s
    }

    def exitWithSemanticErr() :Boolean = {
        semanticErrExist
    }

    def printErr() = {
        errors.foreach(println)
    }
}
