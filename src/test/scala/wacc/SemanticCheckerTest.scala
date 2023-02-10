// package wacc

// import org.scalatest.flatspec.AnyFlatSpec
// import org.scalatest.matchers.should.Matchers._

//   object  ValidNodes {
//   val validIntAssignIdentNode = AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123))
//   val validIdentAssignIdentNode = StatJoinNode(List[StatNode](
//                                       AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123)),
//                                       AssignIdentNode(new BaseTypeNode("int"), new IdentNode("y"), IntLiterNode(456)),
//                                       LValuesAssignNode(new IdentNode("x"), IdentNode("y"))
//                                       ))
  
// }

// object InvalidNodes {
//   val invalidIntAssignIdentNode = AssignIdentNode(new BaseTypeNode("bool"), new IdentNode("x"), IntLiterNode(123))
//   val invalidIdentAssignIdentNode1 =  StatJoinNode(List[StatNode](
//                                       AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123)),
//                                       LValuesAssignNode(new IdentNode("x"), IdentNode("x"))
//                                     ))
//   val invalidIdentAssignIdentNode2 =  StatJoinNode(List[StatNode](
//                                       AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123)),
//                                       AssignIdentNode(new BaseTypeNode("bool"), new IdentNode("y"), BoolLiterNode(false)),
//                                       LValuesAssignNode(new IdentNode("x"), IdentNode("y"))
//                                     ))
//   val invalidIdentAssignIdentNode3 =  StatJoinNode(List[StatNode](
//                                       AssignIdentNode(new BaseTypeNode("int"), new IdentNode("x"), IntLiterNode(123)),
//                                       LValuesAssignNode(new IdentNode("x"), IdentNode("y"))
//                                     ))
//   val invalidIdentAssignIdentNode4 =  StatJoinNode(List[StatNode](
//                                       AssignIdentNode(new BaseTypeNode("bool"), new IdentNode("y"), BoolLiterNode(false)),
//                                       LValuesAssignNode(new IdentNode("x"), IdentNode("y"))
//                                     ))                        
  
// }

// class SemanticCheckerTest extends AnyFlatSpec {
//   "findType (rVal)" should "find correct basic type literals rhs val type" in {
//     SemanticChecker.resetSemanticChecker()
//     /* basic type literals (int, bool, char, string) */
//     assert(SemanticChecker.findTypeR(new IntLiterNode(123)) == "int")
//     assert(SemanticChecker.findTypeR(new BoolLiterNode(true)) == "bool")
//     assert(SemanticChecker.findTypeR(new StrLiterNode("string")) == "str")
//     assert(SemanticChecker.findTypeR(new CharLiterNode('c')) == "char")

//     assert(SemanticChecker.findTypeR(new IntLiterNode(123)) != "bool")
//     assert(SemanticChecker.findTypeR(new BoolLiterNode(true)) != "str")    
//   }

//   it should "find correct identifier (var and func) rhs val type" in {
//     /* identifier (var and func) */
//     SemanticChecker.resetSemanticChecker()
//     ValidNodes.validIdentAssignIdentNode.semanticCheck()
//     assert(SemanticChecker.symbolTable.lookUp("v0!a") == None)
//     assert(SemanticChecker.symbolTable.lookUp("v0!x") != None)
//     assert(SemanticChecker.symbolTable.lookUp("v0!y") != None)
//   }

//   "findType (lVal)" should "find correct basic type literals lhs val type" in {
//     /* identifier (var and func) */
//     SemanticChecker.resetSemanticChecker()


//     val fakeIdNode = new IdentNode("y")
//     fakeIdNode.symbolTableName = "v0!y"
//     assert(SemanticChecker.findTypeL(fakeIdNode) == "ERROR")   
//   }

//   "typeCheck" should "return true for two elem of the same type" in {
//     SemanticChecker.resetSemanticChecker()
//     assert(SemanticChecker.typeCheck(BaseTypeNode("bool"), new IntLiterNode(12345)) == false)

//     SemanticChecker.resetSemanticChecker()
//     assert(SemanticChecker.typeCheck(BaseTypeNode("int"), new IntLiterNode(12345)) == true)
//   }

//   /* ************** AST NODE TESTS ************** */
//   "AssignIdentNode semanticCheck of int" should "return true for valid assignments" in {
//     SemanticChecker.resetSemanticChecker()
//     ValidNodes.validIntAssignIdentNode.semanticCheck()
//     assert(SemanticChecker.errorMessage == "")

//     SemanticChecker.resetSemanticChecker()
//     InvalidNodes.invalidIntAssignIdentNode.semanticCheck()
//     assert(SemanticChecker.errorMessage != "")
//   }

//   "LValuesAssignNode semanticCheck" should "show error for reassignments" in {
//     SemanticChecker.resetSemanticChecker()
//     InvalidNodes.invalidIdentAssignIdentNode1.semanticCheck()
//     assert(SemanticChecker.errorMessage == "Reassignment to same variable: x to x\n")
//   }

//   it should "show error when type mismatch" in {
//     SemanticChecker.resetSemanticChecker()
//     InvalidNodes.invalidIdentAssignIdentNode2.semanticCheck()
//     assert(SemanticChecker.errorMessage == "LHS type \"int\" does not match RHS type \"bool\"\n")
//   }

//   it should "show error when lhs or rhs is undefinded" in {
//     SemanticChecker.resetSemanticChecker()
//     InvalidNodes.invalidIdentAssignIdentNode3.semanticCheck()
//     assert(SemanticChecker.errorMessage == "variable name \"y\" is is not defined in this scope\n")

//     SemanticChecker.resetSemanticChecker()
//     InvalidNodes.invalidIdentAssignIdentNode4.semanticCheck()
//     assert(SemanticChecker.errorMessage == "variable name \"x\" is is not defined in this scope\n")
//   }

  
// }