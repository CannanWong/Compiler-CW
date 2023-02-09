package wacc

sealed trait Identifier {
  def typeEquals(id: Any): Boolean
}

// when ident node not in symbol table
sealed class AnyIdentifier() extends Identifier {
  override def typeEquals(id: Any): Boolean = {
      true
  }
  override def toString(): String = "any"
}

sealed trait BasicTypeIdentifier extends AnyIdentifier

case class IntIdentifier() extends BasicTypeIdentifier {
  override def typeEquals(id: Any): Boolean = {
    id match {
      case i: IntIdentifier => true
      case a: AnyIdentifier => true
      case _ => false
    }
  }
  override def toString(): String = "int"
}

case class BoolIdentifier() extends BasicTypeIdentifier {
  override def typeEquals(id: Any): Boolean = {
    id match {
      case b: BoolIdentifier => true
      case a: AnyIdentifier => true
      case _ => false
    }
  }
  override def toString(): String = "boolean"
}

case class StrIdentifier() extends BasicTypeIdentifier {
  override def typeEquals(id: Any): Boolean = {
    id match {
      case str: StrIdentifier => true
      case a: AnyIdentifier => true      
      case _ => false
    }
  }
  override def toString(): String = "string"
}

case class CharIdentifier() extends BasicTypeIdentifier {
  override def typeEquals(id: Any): Boolean = {
    id match {
      case c: CharIdentifier => {
        SemanticChecker.errorMessage += "char identified\n"
        true
      }
      case a: AnyIdentifier => true
      case _ => false
    }
  }
  override def toString(): String = "char"
}

case class VarIdentifier(ty: Identifier) extends AnyIdentifier {
  override def typeEquals(id: Any): Boolean = {
    id match {
      case VarIdentifier(vtype) => ty.typeEquals(vtype)
      case _ => false
    }
  }
  override def toString(): String = ty.toString()
}

case class FuncIdentifier(paramtype: List[Identifier], returntype: Identifier) extends AnyIdentifier {
  override def typeEquals(id: Any): Boolean = {
    id match {
      case FuncIdentifier(plist, retType) => {
        val paramsTypeValid = (paramtype.length == plist.length) &&
                              paramtype
                              .zip(plist)
                              .map{case (a: Identifier, b: Identifier) => a.typeEquals(b)}
                              .fold(true)((x, y) => x && y)
        paramsTypeValid && returntype.typeEquals(retType)
      }
      case _ => false
    }
  }

  override def toString(): String = returntype.toString()
}

case class ArrayIdentifier(baseTy: Identifier, dim: Int) extends AnyIdentifier {
  override def typeEquals(id: Any): Boolean = {
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

case class PairIdentifier(ty1: Identifier, ty2: Identifier) extends AnyIdentifier {
  override def typeEquals(id: Any): Boolean = {
    id match {
      case PairIdentifier(otherTy1, otherTy2) => {
        ty1.typeEquals(otherTy1) && ty2.typeEquals(otherTy2)
      }
      case a: AnyIdentifier => true
      case _ => false
    }
  }

  override def toString(): String = {
    s"(${ty1.toString()},${ty2.toString()})"
  }
}