package de.tuberlin.uebb.smith.modules
import org.kiama.output.PrettyPrinter
import scalaz._
import Scalaz._
import TypeSynonyms._
import Syntax._
import Unification._
import LookUp._
import TermRewriter._

trait TypeResult {

  abstract class TypeResult
  case class Ok(t: Type) extends TypeResult
  case class FailWrapper(f: Fail) extends Exception
  case class Fail(t: Type, err: List[Error]) extends TypeResult
  implicit class TypeFoo(t1: Type) {
    def &=(t2: Type): Boolean = (t1, t2) match {
      case (UnknownType, _) => true
      case (_, UnknownType) => true
      case (x, y)           => x == y
    }
  }

  implicit class RichTypeChecker(tc1: TypeResult) {
    def &(tc2: TypeResult): TypeResult = tc1 & { _ => tc2 }
    def &(tc2: Type => TypeResult): TypeResult = tc1 match {
      case Ok(t) => tc2(t)
      case Fail(t, err) => tc2(t) match {
        case Ok(t2)         => Fail(t2, err)
        case Fail(t2, err2) => Fail(t2, err ++ err2)
      }
    }
    /**
     * We can't use Fail with kiama, so
     * if we get an error, we wrap and throw him.
     * When we catch him, after we used kiama, we should switch to Fail again
     */
    def $$(): Type = tc1 match {
      case tc1: Ok          => tc1.t
      case f @ Fail(t, err) => throw new FailWrapper(f)
    }
  }

  def lift(t: Type) = Ok(t)
  def error(att: Attribute, e: String) = Fail(UnknownType, List(TypeError(e, att)))
  def error(att: Attribute ,e: String*) = Fail(UnknownType, e.toList.map(TypeError(_, att)))
  def continue() = lift(UnknownType)
}
