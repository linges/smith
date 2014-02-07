package de.tuberlin.uebb.smith.modules

import scalaz._
import Scalaz._
import TypeSynonyms._
abstract class Error(val msg: String, val att: Attribute, name: String) {
  /**
   * Concat errors.
   */
  def &(other: Error): ErrorConcate = ErrorConcate(List(this, other))
  override def toString(): String = name + " error " + att + ": \n" + msg 
}

case class ExceptionWrapper[B](e: Failure[NonEmptyList[Error], B]) extends Exception
/**
  * We can't use Validation with kiama, so
  * if we get an error, we wrap and throw him.
  * When we catch him, after we used kiama, we should switch to Validation again
  */
object ExceptionWrapper {
  def apply[R](r: Result[R]): R = r match {
    case f @ Failure(_) => throw new ExceptionWrapper(f)
    case Success(x)     => x
  }

}

case class ParseError(s: String, override val att: Attribute) extends Error(s,att, "Parser")
case class AdaptiveParseError(s: String, override val att: Attribute) extends Error(s,att, "Parser")
case class AmbError(s: String, override val att: Attribute) extends Error(s,att, "Ambiguity")
case class SDFGeneratorError(s: String, override val att: Attribute) extends Error(s,att, "SDFGenerator")
case class ModuleError(s: String, override val att: Attribute) extends Error(s,att, "Module")
case class ContextError(s: String, override val att: Attribute) extends Error(s,att, "Context")
case class TypeError(s: String, override val att: Attribute) extends Error(s,att, "Type")
case class KindError(s: String, override val att: Attribute) extends Error(s,att, "Kind")
case class InterpreterError(s: String, override val att: Attribute) extends Error(s,att, "Interpreter")
case class InternalError(s: String, override val att: Attribute) extends Error(s,att, "Internal")
case class DesugarError(s: String, override val att: Attribute) extends Error(s,att, "Desugar")
case class ErrorConcate(errors: List[Error]) extends Error("concat",EmptyAttribute, "concat") {
  override def &(other: Error) = other match {
    case ErrorConcate(l) => ErrorConcate(errors ++ l)
    case _               => ErrorConcate(errors :+ other)
  }

  override def toString() : String = errors.mkString("", "\n", "")
}
