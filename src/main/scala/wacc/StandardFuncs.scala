package wacc

import wacc.Constants._
import wacc.Constants.StdFuncsEnum._

// abstract class StandardFuncs(name: String) {
//     import StandardFuncs._
//     protected var used = false
//     def setUsed: Unit = {
//         used = true
//     }
//     def getUsed = used
//     def getFunc: FuncBlock = getFunction(name)

// }


// case object ZeroDivision extends StandardFuncs(ZERO_DIVISION_LABEL)
// case object NullPointer extends StandardFuncs(NULL_POINTER_LABEL)
// case object Overflow extends StandardFuncs(OVERFLOW_LABEL)
// case object BoundsCheck extends StandardFuncs(BOUNDS_CHECK_LABEL)
// case object ArrayStore extends StandardFuncs(ARRAY_STORE_LABEL) {
//     override def setUsed: Unit = {
//         this.used = true
//         BoundsCheck.setUsed
//     }
// }
// case object ArrayStoreB extends StandardFuncs(ARRAY_STORE_B_LABEL) {
//     override def setUsed: Unit = {
//         this.used = true
//         BoundsCheck.setUsed
//     }
// }
// case object ArrayLoad extends StandardFuncs(ARRAY_LOAD_LABEL) {
//     override def setUsed: Unit = {
//         this.used = true
//         BoundsCheck.setUsed
//     }
// }
// case object ArrayLoadB extends StandardFuncs(ARRAY_LOAD_B_LABEL) {
//     override def setUsed: Unit = {
//         this.used = true
//         BoundsCheck.setUsed
//     }
// }
// case object FreePair extends StandardFuncs(FREE_PAIR_LABEL) {
//     override def setUsed: Unit = {
//         this.used = true
//         NullPointer.setUsed
//     }
// }

object StandardFuncs {
    var usedFuncs = new Array[Boolean](StdFuncsEnum.maxId)

    def setUsed(func: StdFuncsEnum): Unit = {
        func match {
            case ArrLdr | ArrLdrB | ArrStr | ArrStrb => {
                setUsed(BoundsErr)
            }
            case FreeP => {
                setUsed(NullErr)
            }
            case _ => 
        }
        usedFuncs(func.id) = true
        
    }

    def getFunction(func: StdFuncsEnum): FuncBlock = {
        func match {
            case ArrLdr | ArrLdrB => {
                val funcBlock = FuncBlock()
                funcBlock.name = func.toString()
                funcBlock.body.addInst(
                    PushInst(lr),
                    CmpInst(r10, ImmVal(0)),
                    MovCondInst("lt", r1, r10),
                    BranchLinkCondInst("lt", "_boundsCheck"),
                    LdrInst(lr, ImmOffset(r8, INT_SIZE)),
                    CmpInst(r10, lr),
                    MovCondInst("ge", r1, r10),
                    BranchLinkCondInst("ge", "_boundsCheck"),
                    func match {
                        case ArrLdr => LdrInst(r8, ScaledOffsetLSL(r8, r10, ImmVal(2)))
                        case ArrLdrB => LdrInst(r8, RegOffset(r8, r10))
                    },
                    PopInst(pc)
                )
                funcBlock
            }
            case ArrStr | ArrStrb => {
                val funcBlock = FuncBlock()
                funcBlock.name = func.toString()
                funcBlock.body.addInst(
                    PushInst(lr),
                    CmpInst(r10, ImmVal(0)),
                    MovCondInst("lt", r1, r10),
                    BranchLinkCondInst("lt", "_boundsCheck"),
                    LdrInst(lr, ImmOffset(r9, INT_SIZE)),
                    CmpInst(r10, lr),
                    MovCondInst("ge", r1, r10),
                    BranchLinkCondInst("ge", "_boundsCheck"),
                    func match {
                        case ArrStr => StrInst(r8, ScaledOffsetLSL(r9, r10, ImmVal(2)))
                        case ArrStrb => StrbInst(r8, RegOffset(r9, r10))
                    },
                    PopInst(pc)
                )
                funcBlock
            }
            case FreeP => {
                val funcBlock = FuncBlock()
                funcBlock.name = func.toString()
                funcBlock.body.addInst(
                    PushInst(lr),
                    MovInst(r8, r0),
                    CmpInst(r8, ImmVal(0)),
                    BranchLinkCondInst("eq", "_errNull"),
                    LdrInst(r0, ImmOffset(r8, 0)),
                    BranchLinkInst("free"),
                    LdrInst(r0, ImmOffset(r8, 4)),
                    BranchLinkInst("free"),
                    MovInst(r0, r8),
                    BranchLinkInst("free"),
                    PopInst(pc)
                )
                funcBlock
            }
            case ZeroDivErr | NullErr | OverflowErr | BoundsErr => {
                RuntimeCheck.runtimeErrorMsg(func.toString())
            }
            // case _ => throw new IllegalArgumentException(s"standard function ${name} does not exist")
        }
    }
}