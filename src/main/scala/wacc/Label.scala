package wacc

import scala.collection.mutable.ListBuffer

abstract class Labels() {
  var name = ""
  var labelCount = 0
  val labelContents = ListBuffer[String]()

  def addToPrintDataSubsection(msg: String) : Unit
}

case class dataDirectiveStat() extends Labels {
  def addToPrintDataSubsection(msg: String) : Unit = {
      labelCount += 1
      labelContents.addOne(msg)
    }

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
