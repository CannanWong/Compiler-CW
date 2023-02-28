package wacc

import scala.collection.mutable.ListBuffer

abstract class Labels() {
  var name = ""
  var labelCount = 0
  val labelContents = ListBuffer[String]()

  def addToPrintDataSubsection(msg: String) : Unit = {
    labelCount += 1
    labelContents.addOne(msg)
  }
  def addPrintLabelToData(text: String, printType: String) : String = {
      val content = new StringBuilder()
      content ++= s"  .word ${text.length()}\n" +
                  s".L.${printType}_str${labelCount}:\n" +
                  s"  .asciz \"${text}\"\n" +
                  ".text\n"
      addToPrintDataSubsection(content.toString())
      // returns label string for text added to .data directive
      s".L.${printType}_str${labelCount}"
    }
}

case class dataDirectiveStat() extends Labels {
  def build(): String = {
    val sb = new StringBuilder()
    sb.++=(".data")
    for (stat <- labelContents) {
      sb.++=(stat)
    } 
    sb.++=(".text")
    sb.toString()
  }
}
