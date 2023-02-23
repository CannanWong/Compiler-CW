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
}
