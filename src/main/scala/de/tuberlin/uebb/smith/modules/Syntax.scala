package de.tuberlin.uebb.smith.modules

import TypeSynonyms._
import scalaz._
import Scalaz._
import scala.collection.immutable.ListMap

object Syntax {
  implicit def string2Id(s: String): Identifier = {
    val as = s.split("\\.")
    if (as.size == 1)
      ID(as(0))
    else
      QID(as.toList)
  }
  implicit def list2Id(s: List[String]): Identifier = {
    if (s.size == 1)
      ID(s(0))
    else
      QID(s.toList)
  }
  implicit def string2Lexeme(s: String): Lexeme = Lexeme(s)
}

abstract class Identifier extends ImportedIdentifier {
  def ~(i: Identifier): QID
  def toPath(): String
}
case class QID(qid: List[String]) extends Identifier {
  override def toString(): String = qid.mkString("", ".", "")
  def ~(i: Identifier): QID = i match {
    case q: QID => QID(qid ++ q.qid)
    case id: ID => QID(qid :+ id.id)
  }
  def toPath() = qid.mkString("", "/", "")
}
case class ID(id: String) extends Identifier {
  override def toString(): String = "" + id + ""
  def ~(i: Identifier): QID = i match {
    case q: QID => QID(id :: q.qid)
    case x: ID  => QID(List(id, x.id))
  }
  def toPath() = id
}
/**
 * ShowId is used in Expression und Types to store names for pretty printing,
 * after removing vars and replacing them with indices.
 * They have a special equals method, so
 * e.g. FORALL X::*. 0 is equal to FORALL Y::*. 0 .
 */
class ShowId(val id: String) extends Identifier {
  override def toString() = "" + id + ""
  override def equals(arg: Any) = arg match {
    case si: ShowId => true
    case _          => false
  }
  override def hashCode() = id.hashCode
  def this(x: Identifier) = this(x match {
    case i: ShowId => i.id
    case _         => x.toString
  })
  def ~(i: Identifier) = throw new IllegalArgumentException("Function on ShowId not allowed" + toString())
  def toPath() = throw new IllegalArgumentException("Function on ShowId not allowed" + toString())
}

case class ModuleHeader(name: Identifier, imports: List[Import], rest: String)(val offset: Position, val fileName : String = "Unknown")
case class Module(name: Identifier, imports: List[Import], typeDef: Map[Identifier, TypeDef], valDef: ListMap[Identifier, ValDef],
                  binderDef: Map[Identifier, BinderDef], bindingDef: Map[Identifier, BindingDef], bracketDef: Map[Identifier, BracketDef], dic: Dictionary)

abstract class Visibility
case object Public extends Visibility
case object Private extends Visibility

/*
 *  Import
 */
case class Import(visibility: Visibility, importedModule: ImportedModule, importModifications: List[ImportModification], att: Attribute = EmptyAttribute){
  override def toString() = SyntaxPrettyPrinter.pretty(this)
}
abstract class ImportedModule(val name: Identifier)
case class NotQualified(override val name: Identifier) extends ImportedModule(name)
case class Qualified(override val name: Identifier) extends ImportedModule(name)
case class QualifiedAs(override val name: Identifier, as: String) extends ImportedModule(name)

abstract class ImportModification
case class ImportRenaming(ren: List[IdentifierRenaming]) extends ImportModification

case class IdentifierRenaming(from: Identifier, to: Identifier) extends ImportedIdentifier
{
  override def toString() : String = from + " AS " + to
}

abstract class ImportRestriction extends ImportModification
case class Only(onlys: List[ImportedIdentifier]) extends ImportRestriction
case class Without(wos: List[Identifier]) extends ImportRestriction

trait ImportedIdentifier

abstract class Def(val visibility: Visibility, val name: Identifier, val att: Attribute) {
  def isPublic(): Boolean = visibility == Public
  override def toString() = SyntaxPrettyPrinter.pretty(this)
}

case class DefAmb(a: Def, b: Def) extends Def(a.visibility, a.name, a.att)
case class ValDef(override val visibility: Visibility, override val name: Identifier, e: Expr, syn: Option[List[AppSyntaxElement]] = None, override val att: Attribute = EmptyAttribute) extends Def(visibility, name, att)
case class TypeDef(override val visibility: Visibility, override val name: Identifier, t: Type, syn: Option[List[AppSyntaxElement]] = None, override val att: Attribute = EmptyAttribute) extends Def(visibility, name, att)
case class BinderDef(override val visibility: Visibility, binderType: BinderType, override val name: Identifier,
                     u: Type, w: Type, e: Expr, syn: Option[List[BinderSyntaxElement]] = None, override val att: Attribute = EmptyAttribute) extends Def(visibility, name, att)
case class BindingDef(override val visibility: Visibility, override val name: Identifier,
                      v: Type, r: Type, forId: Identifier, e: Expr, syn: Option[List[BindingSyntaxElement]] = None, override val att: Attribute = EmptyAttribute) extends Def(visibility, name, att)
case class BracketDef(override val visibility: Visibility, override val name: Identifier, l: BracketLhs, r: BracketRhs, override val att: Attribute = EmptyAttribute) extends Def(visibility, name, att)

abstract class BinderType
case object Seq extends BinderType
case object Par extends BinderType

