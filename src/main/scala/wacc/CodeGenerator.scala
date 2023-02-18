package wacc

import scala.collection.mutable.ListBuffer

object CodeGenerator {
    var controlFlowGraph = new InstBlock()
    var currBlock = controlFlowGraph

    def translate(f: FuncNode): Unit = {
        val funcBlock = new FuncBlock()

        // Sample: To be corrected/checked
        val regList: ListBuffer[Register] = ListBuffer.empty
        for (p <- f.paramList.paramList) {
            regList += new Register()
        }
        funcBlock.param.addInst(PushInst(regList.toList))
        //

        currBlock = funcBlock.body
        translate(f.stat)
    }

    def translate(node: StatNode): Unit = {
        node match {
            case SkipNode() | AssignIdentNode(_,_,_) | LValuesAssignNode(_,_) | ReadNode(_) | 
                 FreeNode(_) | ReturnNode(_) | ExitNode(_) | PrintNode(_) | PrintlnNode(_) | 
                 IfNode(_,_,_) | WhileNode(_,_) | BeginEndNode(_) | StatJoinNode(_) => 
                translate(node)
        }
    }
    def translate(node: SkipNode): Unit = {}
    def translate(node: AssignIdentNode): Unit = {}
    def translate(node: LValuesAssignNode): Unit = {}
    def translate(node: ReadNode): Unit = {}
    def translate(node: FreeNode): Unit = {}
    def translate(node: ReturnNode): Unit = {}
    def translate(node: ExitNode): Unit = {}
    def translate(node: PrintNode): Unit = {}
    def translate(node: PrintlnNode): Unit = {}
    def translate(node: IfNode): Unit = {
        val ifBlock = new IfBlock()
        currBlock = ifBlock.cond
        // translate cond
        currBlock = ifBlock.nextT
        translate(node.fstStat)
        currBlock = ifBlock.nextF
        translate(node.sndStat)
    }
    def translate(node: WhileNode): Unit = {}
    def translate(node: BeginEndNode): Unit = {
        translate(node.stat)
    }
    def translate(node: StatJoinNode): Unit = {
        for (n <- node.statList) {
            translate(n)
        }
    }

    // To be deleted
    /*
    def translate(r: RValueNode): Unit = {
        r match {
            case AddNode(fstexpr, sndexpr) => {
                val rd = new Register(1)
                val rn = new Register(2)
                val op = new ImmVal(1)
                val inst = new AddInst(rd, rn, op)
                currBlock.addInst(inst)
            }
        }
    }
    */
}
