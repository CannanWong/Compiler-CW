package wacc

import wacc.Constants._

case class StandardFuncs(name: String) {
    import StandardFuncs._
    def getFunc: FuncBlock = getFunction(name)
}

object StandardFuncs {
    def addFunc(name: String): Unit = {
        name match {
            case ZERO_DIVISION_LABEL | NULL_POINTER_LABEL | OVERFLOW_LABEL | BOUNDS_CHECK_LABEL |
                 ARRAY_LOAD_LABEL | ARRAY_LOAD_B_LABEL | ARRAY_STORE_LABEL | ARRAY_STORE_B_LABEL => {
                    if (!CodeGenerator.controlFlowFuncs.contains(name)) {
                        val funcBlock = StandardFuncs(name).getFunc
                        CodeGenerator.controlFlowFuncs += ((name, funcBlock))
                    }
                 }
            case _ => throw new IllegalArgumentException(s"${name} is not a pre-defined function.")
        }
    }

    private def getFunction(name: String): FuncBlock = {
        name match {
            case ARRAY_LOAD_LABEL | ARRAY_LOAD_B_LABEL => {
                var funcBlock = new FuncBlock
                funcBlock.body.addInst(
                    PushInst(lr),
                    CmpInst(r10, ImmVal(0)),
                    MovCondInst("lt", r1, r10),
                    BranchLinkCondInst("lt", "_boundsCheck"),
                    LdrInst(lr, ImmOffset(r8, INT_SIZE)),
                    CmpInst(r10, lr),
                    MovCondInst("ge", r1, r10),
                    BranchLinkCondInst("ge", "_boundsCheck"),
                    name match {
                        case ARRAY_LOAD_LABEL => LdrInst(r8, ScaledOffsetLSL(r8, r10, ImmVal(2)))
                        case ARRAY_LOAD_B_LABEL => LdrInst(r8, RegOffset(r8, r10))
                    },
                    PopInst(pc)
                )
                funcBlock
            }
            case ARRAY_STORE_LABEL | ARRAY_STORE_B_LABEL=> {
                var funcBlock = new FuncBlock
                funcBlock.body.addInst(
                    PushInst(lr),
                    CmpInst(r10, ImmVal(0)),
                    MovCondInst("lt", r1, r10),
                    BranchLinkCondInst("lt", "_boundsCheck"),
                    LdrInst(lr, ImmOffset(r9, INT_SIZE)),
                    CmpInst(r10, lr),
                    MovCondInst("ge", r1, r10),
                    BranchLinkCondInst("ge", "_boundsCheck"),
                    name match {
                        case ARRAY_LOAD_LABEL => LdrInst(r8, ScaledOffsetLSL(r9, r10, ImmVal(2)))
                        case ARRAY_LOAD_B_LABEL => LdrInst(r8, RegOffset(r9, r10))
                    },
                    PopInst(pc)
                )
                funcBlock
            }
            case ZERO_DIVISION_LABEL | NULL_POINTER_LABEL | OVERFLOW_LABEL | BOUNDS_CHECK_LABEL => {
                RuntimeCheck.runtimeErrorMsg(name)
            }
            case _ => throw new IllegalArgumentException(s"standard function ${name} does not exist")
        }
    }
}
