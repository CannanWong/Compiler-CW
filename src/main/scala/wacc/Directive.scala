package wacc

import scala.collection.mutable.ListBuffer

/* Constructor class for the directives used in Assembly ARM programs */

abstract class Directive() {
  var GLOBAL_MAIN = false
  var name = ""
  var dirCount = 0
  val dirContents = ListBuffer[String]()

  def build(): String = {
    val ret = new StringBuilder()
    for (c <- dirContents) {
      ret ++= c
    }

    if (GLOBAL_MAIN) {
      ret ++= (".global main\n")
    }
    ret.toString()
  }
}

/**
  * constructor class for creating text directive of the program
  * text section is a part of the program's memory that contains the executable
  * instructions, and the text directives are used to add or modify these instructions.
 */

case class DataDirectiveStat() extends Directive {
  def addToPrintDataSubsection(msg: String) : Unit = {
    dirCount += 1
    dirContents.addOne(msg)
  }

  def addTextLabelToData(text: String) : String = {
    addTextLabelToData(text, "")
  }

  def addTextLabelToData(text: String, printType: String) : String = {
    val content = new StringBuilder()
    val textLabel = if (printType.isEmpty()) "" else s"${printType}_"
    val retLabel = s".L.${textLabel}str${dirCount}"
    val rawText = raw"${text}"
    content ++= s"  .word ${text.length()}\n" +
                s"${retLabel}:\n" +
                s"  .asciz \"${rawText}\"\n"
    addToPrintDataSubsection(content.toString())
    /* returns label string for text added to .data directive */
    retLabel    
  }

  override def build(): String = {
    val sb = new StringBuilder()
    sb.++=("\n.data\n")
    for (stat <- dirContents) {
      sb.++=(stat)
    } 
    sb.++=(".text\n")

    if (GLOBAL_MAIN) {
      sb.++=(".global main\n")
    }
    sb.toString()
  }
}
