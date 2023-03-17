package wacc

sealed trait TypeIdentifier {
  def typeEquals(id: TypeIdentifier): Boolean
  // false if does can be assigned to multiple types
  def isFullType():Boolean
  // true for type alias or non-full type
  def isAbstract():Boolean = false
  def concretenessIndex: Int
  override def toString(): String = "NO TYPE"
}

// when ident node not in symbol table
case class AnyIdentifier() extends TypeIdentifier {
  override def isAbstract():Boolean = true
  def concretenessIndex = 1
  override def isFullType() = false
  override def typeEquals(id: TypeIdentifier): Boolean = {
    true
  }
  override def toString(): String = "any"
}

case class IntIdentifier() extends TypeIdentifier {
  def concretenessIndex = 1
  override def isFullType() = true
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
  def concretenessIndex = 1
  override def isFullType() = true
  override def typeEquals(id: TypeIdentifier): Boolean = {
      id match {
      case BoolIdentifier() => true
      case AnyIdentifier() => true
      case _ => false
    }
  }
  override def toString(): String = "bool"
}

case class StrIdentifier() extends TypeIdentifier {
  def concretenessIndex = 1
  override def isFullType() = true
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
  def concretenessIndex = 1
  override def isFullType() = true
  override def typeEquals(id: TypeIdentifier): Boolean = {
  id match {
      case CharIdentifier() => true
      case AnyIdentifier() => true
      case _ => false
    }
  }
  override def toString(): String = "char"
}

case class FuncIdentifier(old_name: String, paramtype: List[TypeIdentifier], returntype: TypeIdentifier) extends TypeIdentifier {
  def concretenessIndex = 0
  override def isFullType() = true

  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case FuncIdentifier(_, plist, retType) => {
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
  def concretenessIndex = baseTy.concretenessIndex
  override def isFullType() = baseTy.isFullType()
  override def isAbstract(): Boolean = baseTy.isAbstract()

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
    for (i <- 1 to dim) {
      sb.++=("[]")
    }
    s"${baseTy.toString()}${sb.toString()}"
  }
}

case class PairIdentifier(ty1: TypeIdentifier, ty2: TypeIdentifier) extends TypeIdentifier {
  def concretenessIndex = ty1.concretenessIndex + ty2.concretenessIndex
  override def isFullType() = ty1.isFullType() && ty2.isFullType()
  override def isAbstract() = ty1.isAbstract() || ty2.isAbstract()

  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case PairIdentifier(otherTy1, otherTy2) => {
        ty1.typeEquals(otherTy1) && ty2.typeEquals(otherTy2)
      }
      case a: AnyIdentifier => true
      case n: NullIdentifier => true
      case l: ListIdentifier => {
        // right-most element if a list type must be null
        ty1.typeEquals(l.baseTy) && ty2.typeEquals(l)
      }
      case _ => false

    }
  }

  override def toString(): String = {
    s"(${ty1.toString()},${ty2.toString()})"
  }
}

// type alias for pair(<type>, pair(<type>, pair(......, null))
case class ListIdentifier(baseTy: TypeIdentifier) extends TypeIdentifier {
  def concretenessIndex = 2
  override def isAbstract() = true
  def isFullType() = true
  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case n: NullIdentifier => true
      case a: AnyIdentifier => true
      case ListIdentifier(base) => base.typeEquals(baseTy)
      case PairIdentifier(ty1, ty2) => {
        val ty2IsValid: Boolean = ty2 match {
          // right-most element if a list type must be null
          case p@PairIdentifier(innerTy1, innerTy2) => {
            p.isAbstract() && innerTy2.typeEquals(this)
          }
          case NullIdentifier() => true
          case ListIdentifier(nestedBase) => nestedBase.typeEquals(baseTy)
          case _ => false
        }
        ty1.typeEquals(baseTy) && ty2IsValid
    }
    case _ => false
  }
}
  override def toString(): String = s"${baseTy}list"
}
case class NullIdentifier() extends TypeIdentifier {
  def concretenessIndex = 1
  override def isAbstract() = true
  def isFullType() = false
  override def typeEquals(id: TypeIdentifier): Boolean = {
    id match {
      case n: NullIdentifier => true
      case a: AnyIdentifier => true
      case p: PairIdentifier => true
      case l: ListIdentifier => true
      case _ => false
    }
  }

  override def toString(): String = "null"
}