package wacc

import wacc.Constants._

object StandardFuncs {
    def getFunction(name: String): FuncBlock = {
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

object ZeroDivision {
    val name = ZERO_DIVISION_LABEL
    private var used = false
    def setUsed(): Unit = {
        used = true
    }
    def getUsed(): Boolean = {
        used
    }
}

object NullPointer {
    val name = NULL_POINTER_LABEL
    private var used = false
    def setUsed(): Unit = {
        used = true
    }
    def getUsed(): Boolean = {
        used
    }
}

object Overflow {
    val name = OVERFLOW_LABEL
    private var used = false
    def setUsed(): Unit = {
        used = true
    }
    def getUsed(): Boolean = {
        used
    }
}

object BoundsCheck {
    val name = BOUNDS_CHECK_LABEL
    private var used = false
    def setUsed(): Unit = {
        used = true
    }
    def getUsed(): Boolean = {
        used
    }
}

object ArrayLoad {
    val name = ARRAY_LOAD_LABEL
    private var used = false
    def setUsed(): Unit = {
        used = true
        BoundsCheck.setUsed()
    }
    def getUsed(): Boolean = {
        used
    }
}

object ArrayLoadB {
    val name = ARRAY_LOAD_B_LABEL
    private var used = false
    def setUsed(): Unit = {
        used = true
        BoundsCheck.setUsed()
    }
    def getUsed(): Boolean = {
        used
    }
}

object ArrayStore {
    val name = ARRAY_STORE_LABEL
    private var used = false
    def setUsed(): Unit = {
        used = true
        BoundsCheck.setUsed()
    }
    def getUsed(): Boolean = {
        used
    }
}

object ArrayStoreB {
    val name = ARRAY_STORE_B_LABEL
    private var used = false
    def setUsed(): Unit = {
        used = true
        BoundsCheck.setUsed()
    }
    def getUsed(): Boolean = {
        used
    }
}