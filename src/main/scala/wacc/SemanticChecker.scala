package wacc

class SemanticChecker() {
    var errorMessage = ""

    def check(node: ProgramNode): Unit = {
        val result = node.semanticCheck()
            if (result == "") {
                true
            }
            else {
                result
            }
        }
}