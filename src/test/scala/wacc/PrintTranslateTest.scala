package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ListBuffer


class PrintTranslateTest extends AnyFlatSpec {
  val printIntNode = new PrintNode(new IntLiterNode(12))

  "instruction translate print of type int" should "return correctly" in {
    assert(printIntNode != null)
  }

  "instruction under branch label print of type int" should "return correctly" in {
    assert(printIntNode != null)
  }

  "addPrintLabelToData" should "increase dirCount by one" in {
    val dataDirective = DataDirectiveStat()
    val initialDirCount = dataDirective.dirCount

    val label1 = dataDirective.addPrintLabelToData("Hello, world!", "print")

    assert(dataDirective.dirCount == initialDirCount + 1)
    assert(dataDirective.build().contains(".L.print_str0:"))

    val label2 = dataDirective.addPrintLabelToData("Hello, world!", "print")
    assert(dataDirective.dirCount == initialDirCount + 2)
    assert(dataDirective.build().contains(".L.print_str1:"))
  }
}

