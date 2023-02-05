package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

object  ValidNodes {
  val validIntAssignIdentNode = AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123))
}

object InvalidNodes {
  val invalidIntAssignIdentNode = AssignIdentNode(new BaseTypeNode("bool"), new IdentNode("x"), IntLiterNode(123))
}

class SemanticCheckerTest extends AnyFlatSpec {
  "findType" should "find correct rhs val type" in {
    SemanticChecker.resetSemanticChecker()
    /* basic type literals (int, bool, char, string) */
    assert(SemanticChecker.findType(new IntLiterNode(123)) == "int")
    assert(SemanticChecker.findType(new BoolLiterNode(true)) == "bool")
    assert(SemanticChecker.findType(new StrLiterNode("string")) == "str")
    assert(SemanticChecker.findType(new CharLiterNode('c')) == "char")

    assert(SemanticChecker.findType(new IntLiterNode(123)) != "bool")
    assert(SemanticChecker.findType(new BoolLiterNode(true)) != "str")
    /* identifier (var and func) */
  }

  "typeCheck" should "return true for two elem of the same type" in {
    SemanticChecker.resetSemanticChecker()
    assert(SemanticChecker.typeCheck(BaseTypeNode("bool"), new IntLiterNode(12345)) == false)

    SemanticChecker.resetSemanticChecker()
    assert(SemanticChecker.typeCheck(BaseTypeNode("int"), new IntLiterNode(12345)) == true)
  }

  "AssignIdentNode semanticCheck of int" should "return true for valid assignments" in {
    SemanticChecker.resetSemanticChecker()
    ValidNodes.validIntAssignIdentNode.semanticCheck()
    assert(SemanticChecker.errorMessage == "")

    SemanticChecker.resetSemanticChecker()
    InvalidNodes.invalidIntAssignIdentNode.semanticCheck()
    assert(SemanticChecker.errorMessage == "LHS type \"bool\" does not match RHS type \"int\"")
  }
  
}