package wacc

sealed trait Identifier {
  def typeEquals(id: Identifier): Boolean
  override def toString(): String = "NO TYPE"
}

// when ident node not in symbol table
case class AnyIdentifier() extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
    true
  }
  override def toString(): String = "any"
}

case class IntIdentifier() extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
    id match {
      case IntIdentifier() => true
      case AnyIdentifier() => true
      case VarIdentifier(varId) => {
        this.typeEquals(varId)
      }
      case _ => false
    }
  }
  override def toString(): String = "int"
}

case class BoolIdentifier() extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
      id match {
      case BoolIdentifier() => true
      case AnyIdentifier() => true
      case VarIdentifier(varId) => {
        this.typeEquals(varId)
      }
      case _ => false
    }
  }
  override def toString(): String = "boolean"
}

case class StrIdentifier() extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
  id match {
      case StrIdentifier() => true
      case AnyIdentifier() => true 
      case VarIdentifier(varId) => {
        this.typeEquals(varId)
      }
      case _ => false
    }
  }
  override def toString(): String = "string"
}

case class CharIdentifier() extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
  id match {
      case CharIdentifier() => true
      case AnyIdentifier() => true
      case VarIdentifier(varId) => {
        this.typeEquals(varId)
      }
      case _ => false
    }
  }
  override def toString(): String = "char"
}

case class VarIdentifier(ty: Identifier) extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
    ty.typeEquals(id)
  }
  override def toString(): String = ty.toString()
}

case class FuncIdentifier(paramtype: List[Identifier], returntype: Identifier) extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
    id match {
      case FuncIdentifier(plist, retType) => {
        val paramstypeValid = (paramtype.length == plist.length) &&
                              paramtype
                              .zip(plist)
                              .map{case (a: Identifier, b: Identifier) => a.typeEquals(b)}
                              .fold(true)((x, y) => x && y)
        paramstypeValid && returntype.typeEquals(retType)
      }
      case VarIdentifier(varId) => this.typeEquals(varId)
      case _ => false
    }
  }

  override def toString(): String = returntype.toString()
}
 
case class ArrayIdentifier(baseTy: Identifier, dim: Int) extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
    id match {
      case ArrayIdentifier(tyId, d) => {
        baseTy.typeEquals(tyId) && d == dim
      }
      case VarIdentifier(varId) => this.typeEquals(varId)
      case a: AnyIdentifier => true
      case _ => false
    }
  }
  override def toString(): String = {
    val sb = new StringBuilder()
    val brackets = for (i <- 1 to dim) {
      sb.++=("[]")
    }
    s"${baseTy.toString()}${brackets}"

  }
}

case class PairIdentifier(ty1: Identifier, ty2: Identifier) extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
    id match {
      case PairIdentifier(otherTy1, otherTy2) => {
        ty1.typeEquals(otherTy1) && ty2.typeEquals(otherTy2)
      }
      case a: AnyIdentifier => true
      case n: NullIdentifier => true
      case VarIdentifier(varId) => this.typeEquals(varId)
      case _ => false
    }
  }

  override def toString(): String = {
    s"(${ty1.toString()},${ty2.toString()})"
  }
}

case class NullIdentifier() extends Identifier {
  override def typeEquals(id: Identifier): Boolean = {
    id match {
      case a: AnyIdentifier => true
      case p: PairIdentifier => true
      case VarIdentifier(varId) => this.typeEquals(varId)
      case _ => false
    }
  }

  override def toString(): String = "null"
}