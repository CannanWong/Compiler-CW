package wacc

import scala.collection.mutable.ListBuffer

abstract class Directive() {
  val GLOBAL_MAIN = false
  var name = ""
  var dirCount = 0
  val dirContents = ListBuffer[String]()

  def build(): String = {
    if (GLOBAL_MAIN) {
      dirContents.addOne(".global main")
    }
    val ret = new StringBuilder()
    for (c <- dirContents) {
      ret ++= c
    }
    ret.toString()
  }
}

case class DataDirectiveStat() extends Directive {
  def addToPrintDataSubsection(msg: String) : Unit = {
    dirCount += 1
    dirContents.addOne(msg)
  }

  def addTextLabelToData(text: String, printType: String) : String = {
    val content = new StringBuilder()
    val textLabel = if (printType.isEmpty()) "" else s"${printType}_"
    content ++= s"  .word ${text.length()}\n" +
                s".L.${textLabel}str${dirCount}:\n" +
                s"  .asciz \"${text}\"\n"
    addToPrintDataSubsection(content.toString())

    // returns label string for text added to .data directive
    s".L.${textLabel}str${dirCount}"
  }

  override def build(): String = {
    val sb = new StringBuilder()
    sb.++=(".data\n")
    for (stat <- dirContents) {
      sb.++=(stat)
    } 
    sb.++=(".text")
    sb.toString()
  }
}
