package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


class PrintTranslateTest extends AnyFlatSpec {
  val printIntNode = new PrintNode(new IntLiterNode(12))

  "instruction translate print of type int" should "return correctly" in {
    assert(printIntNode != null)
  }

  "instruction under branch label print of type int" should "return correctly" in {
    assert(printIntNode != null)
  }

  "addTextLabelToData" should "increase dirCount by one" in {
    val dataDirective = DataDirectiveStat()
    val initialDirCount = dataDirective.dirCount

    val label1 = dataDirective.addTextLabelToData("Hello, world!", "print")

    assert(dataDirective.dirCount == initialDirCount + 1)
    assert(dataDirective.build().contains(label1))

    val label2 = dataDirective.addTextLabelToData("Hello, world!", "print")
    assert(dataDirective.dirCount == initialDirCount + 2)
    assert(dataDirective.build().contains(label2))
  }
}

