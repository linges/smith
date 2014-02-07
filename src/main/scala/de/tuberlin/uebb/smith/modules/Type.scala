package de.tuberlin.uebb.smith.modules

import TypeSynonyms._

//proper types
sealed abstract class Type(val att: Attribute) {
  var name = ""
  override def toString() = SyntaxPrettyPrinter.pretty(this)
}
case object UnknownType extends Type(EmptyAttribute)
case class TyArrow(l: Type, r: Type, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyApp(l: Type, r: Type, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyForall(id: Identifier, k: Kind, t: Type, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyAbs(id: Identifier, k: Kind, t: Type, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyInt(override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyString(override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyBool(override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyVar(id: Identifier, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyQVar(id: Identifier, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyRecord(l: Map[String, Type], override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyVariant(l: Map[String, Type], override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyMu(l: Type, r: Type, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyIndex(i: Int, id: ShowId = new ShowId("X"), override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyPair(l: Type, r: Type, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyFst(k: Type, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TySnd(k: Type, override val att: Attribute = EmptyAttribute) extends Type(att)
case class TyUni(id: Identifier, override val att: Attribute = EmptyAttribute) extends Type(att) //Dummy to unify with 
case class TyAmb(a: Type, b: Type, override val att: Attribute = EmptyAttribute) extends Type(att)

sealed abstract class Kind(val att: Attribute) {
  override def toString() = SyntaxPrettyPrinter.pretty(this)
}
case class KiArrow(l: Kind, r: Kind, override val att: Attribute = EmptyAttribute) extends Kind(att)
case class KiStar(override val att: Attribute = EmptyAttribute) extends Kind(att)
case class KiPair(l: Kind, r: Kind, override val att: Attribute = EmptyAttribute) extends Kind(att)
