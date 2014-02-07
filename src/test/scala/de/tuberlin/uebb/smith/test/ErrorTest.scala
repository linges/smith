import de.tuberlin.uebb.smith.modules._
import de.tuberlin.uebb.smith.modules.Syntax._
import org.scalatest.matchers._
import org.scalatest.{ FunSpec, Inside }
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import org.kiama.output.PrettyPrinter
import TypeSynonyms._
import Syntax._
import TermRewriter._
import org.spoofax.interpreter.terms.IStrategoTerm
import org.scalatest._

class ErrorTest extends FunSpec with Inside with ShouldMatchers with AdaptiveParser with SDFGenerator
  with TypeChecker with Driver with PrettyPrinter with DependenceAnalysis with ContextChecker {

  workingDir = "testFiles"

  class ErrorMatcher[T](expected: Error) extends Matcher[String] {
    def apply(left: String) = {

      val result = compile(left, "1").flatMap(Interpreter.run(_))

      val failureMessageSuffix = result match {
        case Failure(e) => "Error: " + e.list.reduce(_ & _)
        case Success(e) => "'" + left + "' returned  \n\t" + e + " but expected was \n\t" + expected.toString
      }

      val negatedFailureMessageSuffix = "TODO negatedFailureMessageSuffix"

      val equals = {
        result.fold[Boolean]((x =>  expected.getClass == x.head.getClass
          && x.head.msg.contains(expected.msg)), (_ => false))
      }

      MatchResult(
        equals,
        failureMessageSuffix,
        negatedFailureMessageSuffix,
        failureMessageSuffix,
        negatedFailureMessageSuffix)
    }
  }

  def be(e: Error) = new ErrorMatcher(e)

  
  describe("Interpreter") {
    it("no branch match") {
      "CaseError" should be(InterpreterError("branch", EmptyAttribute))
    }
    it("char of") {
      "CharOfError" should be(InterpreterError("index", EmptyAttribute))
    }
    it("overflow") {
      "OverflowError" should be(InterpreterError("bound", EmptyAttribute))
    }
  }
  describe("Module") {
    it("wrong module name") {
      "ModuleNameError" should be(ModuleError("but found module", EmptyAttribute))
    }
    it("cycle") {
      "CycleError" should be(ModuleError("Cycle", EmptyAttribute))
    }
  }
  describe("KindChecker") {
    it("selection on pair") {
      "TypePairError" should be(KindError("pair", EmptyAttribute))
    }
    it("wrong argument kind ") {
      "KindAppError" should be(KindError("Argument", EmptyAttribute))
    }
    it("wrong abs kind ") {
      "KindAbsError" should be(KindError("type abstraction", EmptyAttribute))
    }
    it("wrong mu kind ") {
      "KindMuError" should be(KindError("mu", EmptyAttribute))
    }
  }
  describe("TypeChecker") {
    it("type errors") {
      "TypeError" should be(TypeError("Argument", EmptyAttribute))
    }
  }
  describe("SDFGenerator") {
    it("no lexeme in syntax") {
      "NoLexError" should be(SDFGeneratorError("core", EmptyAttribute))
    }
    it("core syntax") {
      "CoreSynError" should be(SDFGeneratorError("core", EmptyAttribute))
    }
    it("syn for non abs") {
      "SynArgError" should be(SDFGeneratorError("function type", EmptyAttribute))
    }
    it("syn for non type abs") {
      "TypeSynArgError" should be(SDFGeneratorError("typeoperator", EmptyAttribute))
    }
    it("var error") {
      "VARError" should be(SDFGeneratorError("VAR", EmptyAttribute))
    }
    it("bindee error") {
      "BINDEEError" should be(SDFGeneratorError("BINDEE", EmptyAttribute))
    }
    it("binding error") {
      "BINDINGError" should be(SDFGeneratorError("BINDING", EmptyAttribute))
    }
    it("unknown syntax error") {
      "BracketNoSynError" should be(SDFGeneratorError("not find syntax", EmptyAttribute))
    }
  }
  describe("Disambiguate") {
    it("ambiguous") {
      "AmbError" should be(AmbError("ambiguous", EmptyAttribute))
    }
    it("ambiguous and errors") {
      "AmbMultiError" should be(AmbError("correct one", EmptyAttribute))
    }
  }
}

