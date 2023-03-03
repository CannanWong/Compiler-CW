package wacc

sealed trait TypeIdentifier {
  def typeEquals(id: TypeIdentifier): Boolean
  override def toString(): String = "NO TYPE"
}

// when ident node not in symbol table
case class AnyIdentifier() extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
    true
  }
  override def toString(): String = "any"
}

case class IntIdentifier() extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case IntIdentifier() => true
      case AnyIdentifier() => true
      case _ => false
    }
  }
  override def toString(): String = "int"
}

case class BoolIdentifier() extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
      id match {
      case BoolIdentifier() => true
      case AnyIdentifier() => true
      case _ => false
    }
  }
  override def toString(): String = "boolean"
}

case class StrIdentifier() extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
  id match {
      case StrIdentifier() => true
      case AnyIdentifier() => true 
      case _ => false
    }
  }
  override def toString(): String = "string"
}

case class CharIdentifier() extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
  id match {
      case CharIdentifier() => true
      case AnyIdentifier() => true
      case _ => false
    }
  }
  override def toString(): String = "char"
}

case class FuncIdentifier(paramtype: List[TypeIdentifier], returntype: TypeIdentifier) extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case FuncIdentifier(plist, retType) => {
        val paramstypeValid = (paramtype.length == plist.length) &&
                              paramtype
                              .zip(plist)
                              .map{case (a: TypeIdentifier, b: TypeIdentifier) => a.typeEquals(b)}
                              .fold(true)((x, y) => x && y)
        paramstypeValid && returntype.typeEquals(retType)
      }
      case _ => false
    }
  }

  override def toString(): String = returntype.toString()
}
 
case class ArrayIdentifier(baseTy: TypeIdentifier, dim: Int) extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case ArrayIdentifier(tyId, d) => {
        baseTy.typeEquals(tyId) && d == dim
      }
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

case class PairIdentifier(ty1: TypeIdentifier, ty2: TypeIdentifier) extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case PairIdentifier(otherTy1, otherTy2) => {
        ty1.typeEquals(otherTy1) && ty2.typeEquals(otherTy2)
      }
      case a: AnyIdentifier => true
      case n: NullIdentifier => true
      case _ => false
    }
  }

  override def toString(): String = {
    s"(${ty1.toString()},${ty2.toString()})"
  }
}

case class NullIdentifier() extends TypeIdentifier {
  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case a: AnyIdentifier => true
      case p: PairIdentifier => true
      case _ => false
    }
  }

  override def toString(): String = "null"
}