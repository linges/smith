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

class EvalTypeTest extends FunSpec with Inside with ShouldMatchers with AdaptiveParser with SDFGenerator
  with TypeChecker with Driver with PrettyPrinter with DependenceAnalysis with ContextChecker {

  override def fail(s: String) = super[FunSpec].fail(s)

  val testModule = """ 
    MODULE test
    VAL varTest = 3
    TYPE List = \A::*. MU(\L::* => *. \B::*. <nil: {}, cons:{hd:B, tl: L B}>,A)
    TYPE IntList = List Int 
    VAL nil = FOLD [IntList] < nil = {} > : < nil: {}, cons: { hd:Int, tl: IntList }>
    VAL unapp = \x:IntList.UNFOLD [IntList] x
    TYPE Pair = \X::*.\Y::*. {y:Y, x:X }
    TYPE Fix = \A::*. \B::*. MU (\R::*=>*. \args::*. R args -> A -> B, {})
"""

  val testContext: Context = {
    val ctx = compile("test", "2")
    ctx.fold(l => println("Test Context error" + l), r => ())
    ctx.toOption.get
  }
  def parseAndEvalType(input: String): Result[Type] = {
    for (
      term <- parseSDF("TYPE x = " + input, "DefRest");
      r <- parseDefRest(term,Position(0,0));
      ty <- evalTypeV(removeNamesFromType(r._1.asInstanceOf[TypeDef].t),
        testContext.st, testContext.modules("test").dic, "test")
    ) yield ty
  }
  override def loadModuleHeader(moduleName: Identifier): ValidationNEL[Error, ModuleHeader] =
    {
      val m = for (
        t <- parseSDF(testModule, "ModuleRest");
        x <- parseModuleHeader(t.asInstanceOf[IStrategoTerm])
      ) yield {
        moduleHeaderCache += x.name -> x
        x
      }
      m
    }
  class EvalTypeMatcher[T](expected: T) extends Matcher[String] {
    def apply(left: String) = {
      val result = parseAndEvalType(left)
      val failureMessageSuffix = result match {
        case Failure(e) => "Error: " + e.list.reduce(_ & _)
        case Success(e) => "'" + left + "' has the calulated type \n\t" + e + " but expected was \n\t" + expected.toString
      }
      val negatedFailureMessageSuffix = "TODO negatedFailureMessageSuffix"
      val equals = result.fold[Boolean]((_ => false), _ == expected)
      MatchResult(
        equals,
        failureMessageSuffix,
        negatedFailureMessageSuffix,
        failureMessageSuffix,
        negatedFailureMessageSuffix)
    }
  }
  def beType(t: String) = {
    val ty = for (
      term <- parseSDF("TYPE x = " + t, "DefRest");
      r <- parseDefRest(term,Position(0,0))
    ) yield r._1.asInstanceOf[TypeDef].t
    ty.fold(e => println(e.toString), r => ())
    new EvalTypeMatcher(removeNamesFromType(ty.toOption.get))
  }

  describe("Test case 1: Basic values") {
    it("Type Int") {
      "Int" should beType("Int")
    }
    it("Type Pair Int Bool") {
      "Pair Int Bool " should beType("{ x:Int, y: Bool }")
    }
    it("Nestes Type Abstratction and application with Variables as Argument") {
      """ \A::*.\B::*. (\C::*. \D::*=>*. D C) B
      """ should beType(
        """ \A::*.\B::*. \D::*=>*. D B """)
    }
    it("Type Application with Abstraction as Argument") {
      """
     \A::*.\B::*.  
     ((\R::*.\args::*. R args -> A -> B)  (\arg::*. MU(\R::*=>*. \args::*. R args -> A -> B, arg)) {}) 
      """ should beType(
        """\A::*.\B::*.
        MU (\R::*=>*. \args::*. R args -> A -> B, {}) -> A -> B""")
    }
  }
}
