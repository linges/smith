package de.tuberlin.uebb.smith.modules

import TypeSynonyms._
sealed abstract class Expr(val att: Attribute) {
  override def toString() = SyntaxPrettyPrinter.pretty(this)
}

case class App(f: Expr, x: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Var(id: Identifier, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class QVar(id: Identifier, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Abs(x: Identifier, t: Type, e: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Closure(x: Identifier, t: Type, e: Expr, dic: Dictionary, local: Map[Identifier, Expr],
  override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Num(n: Int, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Str(s: String, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Bool(b: Boolean, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Cond(c: Expr, t: Expr, e: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class TApp(f: Expr, x: Type, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class TAbs(x: String, k: Kind, e: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Fold(t: Type, e: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Unfold(t: Type, e: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Record(l: Map[String, Expr], override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Selection(e: Expr, s: String, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Variant(s: String, e: Expr, t: Type, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Ascription(e: Expr, t: Type, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class CaseOf(e: Expr, cases: List[Branch], override val att: Attribute = EmptyAttribute) extends Expr(att)
case class LetSeq(id: Identifier, bs: List[Binding], e: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class LetPar(id: Identifier, bs: List[Binding], e: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Add(x: Expr, y: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Sub(x: Expr, y: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Mul(x: Expr, y: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Div(x: Expr, y: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class CharOf(x: Expr, y: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Concat(x: Expr, y: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class Greater(x: Expr, y: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class StrSize(x: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)
case class ExprAmb(a: Expr, b: Expr, override val att: Attribute = EmptyAttribute) extends Expr(att)

case class Branch(a: String, b: String, e: Expr, val att: Attribute = EmptyAttribute)

abstract class Binding(val name: Identifier, val vari: Identifier, val e: Expr, val att: Attribute = EmptyAttribute)
case class BindingN(override val name: Identifier, override val vari: Identifier, override val e: Expr, override val att: Attribute = EmptyAttribute) extends Binding(name, vari, e, att)
case class BindingAs(override val name: Identifier, override val vari: Identifier, val t: Type, override val e: Expr, override val att: Attribute = EmptyAttribute) extends Binding(name, vari, e, att)
case class BindingAmb(x: Binding, y: Binding) extends Binding(ID("dummy"), ID("dummy"), Var(ID("dummy")), EmptyAttribute)
