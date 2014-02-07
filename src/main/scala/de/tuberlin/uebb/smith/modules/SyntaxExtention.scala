package de.tuberlin.uebb.smith.modules

import TypeSynonyms._
import scalaz._
import Scalaz._

case class BracketLhs(l: List[AppSyntaxElement])
case class BracketRhs(l: List[AppSyntaxElement], bracket: Option[BracketUsing] = None)
case class BracketUsing(l: Lexeme, r: Lexeme)

trait SyntaxElement

trait BinderSyntaxElement extends SyntaxElement
case object Bindings extends BinderSyntaxElement
case object LRBindings extends BinderSyntaxElement
case object RLBindings extends BinderSyntaxElement
case object BinderBody extends BinderSyntaxElement

trait BindingSyntaxElement extends SyntaxElement
case object BindingVar extends BindingSyntaxElement
case object BindingBindee extends BindingSyntaxElement

case class Lexeme(s: String) extends BinderSyntaxElement with BindingSyntaxElement with AppSyntaxElement

trait AppSyntaxElement extends SyntaxElement
case object AppArg extends AppSyntaxElement
