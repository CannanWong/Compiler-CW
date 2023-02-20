package wacc

import scala.collection.mutable.ListBuffer

object CodeGenerator {
    var controlFlowGraph = new InstBlock()
    var currInstBlock = controlFlowGraph

    def translate(f: FuncNode): Unit = {
        val funcBlock = new FuncBlock()

        // // Sample: To be corrected/checked
        // val varList: ListBuffer[Variable] = ListBuffer.empty
        // for (p <- f.paramList.paramList) {
        //     regList += new Variable()
        // }
        // funcBlock.param.addInst(PushInst(regList.toList))
        // //

        currInstBlock = funcBlock.body
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
    def translate(node: ExitNode): Unit = {
        // currInstBlock.addInst()
    }
    def translate(node: PrintNode): Unit = {}
    def translate(node: PrintlnNode): Unit = {}
    def translate(node: IfNode): Unit = {
        val ifBlock = new IfBlock()
        currInstBlock = ifBlock.cond
        // translate cond
        currInstBlock = ifBlock.nextT
        translate(node.fstStat)
        currInstBlock = ifBlock.nextF
        translate(node.sndStat)
        currInstBlock = ifBlock.next
    }
    def translate(node: WhileNode): Unit = {
        val whileBlock = new WhileBlock()
        currInstBlock = whileBlock.cond
        // translate cond
        currInstBlock = whileBlock.loop
        translate(node.stat)
    }
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
