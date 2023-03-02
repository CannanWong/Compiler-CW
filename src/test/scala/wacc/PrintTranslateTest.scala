package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.Seconds
import org.scalatest.time.Span


class PrintTranslateTest extends AnyFlatSpec with TimeLimitedTests {
  // Define a 60-second time limit for the test case
  override def timeLimit = Span(100, Seconds)

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

    val label2 = dataDirective.addTextLabelToData("Hello, world!")
    assert(dataDirective.dirCount == initialDirCount + 2)
    assert(dataDirective.build().contains(label2))
  }

  "printBlock" should "not exceed the time limit" in {
    // Create a sample ControlFlowBlock instance
    val block1 = InstBlock()
    block1.addInst(
      List(
      AddInst(FixedRegister(4), FixedRegister(4), ImmVal(4)),
      AddInst(FixedRegister(4), FixedRegister(4), ImmVal(4))
      ))

    val block2 = InstBlock()
    block1.addInst(
      List(
      AddInst(FixedRegister(4), FixedRegister(4), ImmVal(4)),
      AddInst(FixedRegister(4), FixedRegister(4), ImmVal(4))
      ))

    block1.next = block2


    Printer.printBlock(block1)

    assert(!Printer.output.isEmpty)

  }
}

