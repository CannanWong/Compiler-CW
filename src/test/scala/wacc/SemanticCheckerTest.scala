package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

  object  ValidNodes {
  val validIntAssignIdentNode = AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123))
  val validIdentAssignIdentNode = StatJoinNode(List[StatNode](
                                      AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123)),
                                      AssignIdentNode(new BaseTypeNode("int"), new IdentNode("y"), IntLiterNode(456)),
                                      LValuesAssignNode(new IdentNode("x"), IdentNode("y"))
                                      ))
  
}

object InvalidNodes {
  val invalidIntAssignIdentNode = AssignIdentNode(new BaseTypeNode("bool"), new IdentNode("x"), IntLiterNode(123))
  val invalidIdentAssignIdentNode1 =  StatJoinNode(List[StatNode](
                                      AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123)),
                                      LValuesAssignNode(new IdentNode("x"), IdentNode("x"))
                                    ))
  val invalidIdentAssignIdentNode2 =  StatJoinNode(List[StatNode](
                                      AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123)),
                                      AssignIdentNode(new BaseTypeNode("bool"), new IdentNode("y"), BoolLiterNode(false)),
                                      LValuesAssignNode(new IdentNode("x"), IdentNode("y"))
                                    ))
  val invalidIdentAssignIdentNode3 =  StatJoinNode(List[StatNode](
                                      AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123)),
                                      LValuesAssignNode(new IdentNode("x"), IdentNode("y"))
                                    ))
  val invalidIdentAssignIdentNode4 =  StatJoinNode(List[StatNode](
                                      AssignIdentNode(new BaseTypeNode("bool"), new IdentNode("y"), BoolLiterNode(false)),
                                      LValuesAssignNode(new IdentNode("x"), IdentNode("y"))
                                    ))                        
  
}

class SemanticCheckerTest extends AnyFlatSpec {

  it should "find correct identifier (var and func) rhs val type" in {
    /* identifier (var and func) */
    SemanticChecker.resetSemanticChecker()
    ValidNodes.validIdentAssignIdentNode.semanticCheck()
    assert(SemanticChecker.symbolTable.lookUpVar("a") == None)
    assert(SemanticChecker.symbolTable.lookUpVar("x") != None)
    assert(SemanticChecker.symbolTable.lookUpVar("y") != None)
  }

  /* ************** AST NODE TESTS ************** */
  "AssignIdentNode semanticCheck of int" should "return true for valid assignments" in {
    SemanticChecker.resetSemanticChecker()
    ValidNodes.validIntAssignIdentNode.semanticCheck()
    assert(!Error.exitWithSemanticErr())

    SemanticChecker.resetSemanticChecker()
    InvalidNodes.invalidIntAssignIdentNode.semanticCheck()
    assert(Error.exitWithSemanticErr())
  }

  it should "show error when type mismatch" in {
    SemanticChecker.resetSemanticChecker()
    InvalidNodes.invalidIdentAssignIdentNode2.semanticCheck()
    assert(Error.exitWithSemanticErr())
  }

  it should "show error when lhs or rhs is undefinded" in {
    SemanticChecker.resetSemanticChecker()
    InvalidNodes.invalidIdentAssignIdentNode3.semanticCheck()
    assert(Error.exitWithSemanticErr())

    SemanticChecker.resetSemanticChecker()
    InvalidNodes.invalidIdentAssignIdentNode4.semanticCheck()
    assert(Error.exitWithSemanticErr())
  }

}