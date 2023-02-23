package wacc

import scala.collection.mutable.ListBuffer

trait Labels {
  val labelContents: ListBuffer[String]
  var labelCount = 0
  def addToPrintDataSubsection(msg: String) : Unit = {
      labelCount += 1
      labelContents.addOne(msg)
    }
}

case class dataDirectiveStat() extends Labels {
  val labelContents = ListBuffer[String](".data")
  def build(): String = {
    //labelContents.forall()
    ""
  }
}
