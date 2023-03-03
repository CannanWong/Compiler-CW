package wacc

import wacc.Constants._
import wacc.Constants.StdFuncsEnum._


object StandardFuncs {
    var usedFuncs = new Array[Boolean](StdFuncsEnum.maxId)

    /* record the function as used so to add function to program at the end of code generation */
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
    /* returns the standard function inicated by func ENUM parameter */
    def getFunction(func: StdFuncsEnum): FuncBlock = {
        func match {
            case ArrLdr | ArrLdrB => {
                val funcBlock = FuncBlock()
                funcBlock.name = func.toString()
                funcBlock.body.addInst(
                    PushInst(lr),
                    CmpInst(r10, ImmVal(0)),
                    MovCondInst(LESS_THAN, r1, r10),
                    BranchLinkCondInst(LESS_THAN, BOUNDS_CHECK_LABEL),
                    LdrInst(lr, ImmOffset(r8, INT_SIZE)),
                    CmpInst(r10, lr),
                    MovCondInst(GREATER_OR_EQUAL, r1, r10),
                    BranchLinkCondInst(GREATER_OR_EQUAL, BOUNDS_CHECK_LABEL),
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
                    MovCondInst(LESS_THAN, r1, r10),
                    BranchLinkCondInst(LESS_THAN, BOUNDS_CHECK_LABEL),
                    LdrInst(lr, ImmOffset(r9, INT_SIZE)),
                    CmpInst(r10, lr),
                    MovCondInst(GREATER_OR_EQUAL, r1, r10),
                    BranchLinkCondInst(GREATER_OR_EQUAL, BOUNDS_CHECK_LABEL),
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
                    BranchLinkCondInst(EQUAL, NULL_POINTER_LABEL),
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
        }
    }
}