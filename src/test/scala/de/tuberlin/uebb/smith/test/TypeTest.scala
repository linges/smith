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

class TypeTest extends FunSpec with Inside with ShouldMatchers with AdaptiveParser with SDFGenerator
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
    TYPE Fix = \X::*. \Z::*. MU (\R::*=>*. \args::*. R args -> X -> Z, {})
"""

  val testContext: Context = {
    val ctx = compile("test", "2")
    ctx.fold(l => println("Test Context error" + l), r => ())
    ctx.toOption.get
  }
  def typeOfTerm(input: String): Result[Type] = {
    for (
      term <- parseSDF("VAL x = " + input, "DefRest");
      r <- parseDefRest(term,Position(0,0));
      ty <- computeType(removeNamesFromExpr(r._1.asInstanceOf[ValDef].e),
        testContext, testContext.modules("test").dic, "test")
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
  class TypeMatcher[T](expected: T) extends Matcher[String] {
    def apply(left: String) = {

      val result = typeOfTerm(left)

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
  def haveType(t: Type) =
    {
      val et = evalTypeV(removeNamesFromType(t),
        testContext.st, testContext.modules("test").dic, "test")
      et.fold(e => println(e.toString), r => ())
      new TypeMatcher(et.toOption.getOrElse(t))
    }
  def haveType(t: String) = {
    val ty = for (
      term <- parseSDF("TYPE x = " + t, "DefRest");
      r <- parseDefRest(term,Position(0,0))
    ) yield r._1.asInstanceOf[TypeDef].t
    ty.fold(e => println(e.toString), r => ())
    new TypeMatcher(removeNamesFromType(ty.toOption.get))
  }

  describe("Test case 1: Basic values") {
    it("Number should have Type Int") {
      "23" should haveType(TyInt())
    }
    it("Bool should have Type Bool") {
      "TRUE" should haveType(TyBool())
    }
    it("String should have Type String") {
      """"foo"""" should haveType(TyString())
    }
    it("Variablen lookup should have Type Int") {
      "varTest" should haveType(TyInt())
    }
    it("Simple type abstraction") {
      """\X::*.\x:X.x""" should haveType(TyForall("X", KiStar(), TyArrow(TyVar("X"), TyVar("X"))))
    }
    it("Simple type abstraction with different variable names") {
      """\X::*.\x:X.x""" should haveType(TyForall("Y", KiStar(), TyArrow(TyVar("Y"), TyVar("Y"))))
    }
    it("Simple type abstraction applied") {
      """(\X::*.\x:X.x)[Int]""" should haveType(TyArrow(TyInt(), TyInt()))
    }
    it("ADD primitive applied ") {
      """ADD 2 3""" should haveType(TyInt())
    }
    it("SUB primitive applied ") {
      """SUB 2 3""" should haveType(TyInt())
    }
    it("MUL primitive applied ") {
      """MUL 2 3""" should haveType(TyInt())
    }
    it("DIV primitive applied") {
      """DIV 2 3""" should haveType(TyInt())
    }
    it("GT primitive applied") {
      """GT 2 3""" should haveType(TyBool())
    }
    it("CHAROF primitive applied") {
      """CHAROF "foo" 2""" should haveType(TyString())
    }
    it("CONCAT primitive applied") {
      """CONCAT "a" "b" """ should haveType(TyString())
    }
    it("SIZE primitive applied") {
      """SIZE "aa" """ should haveType(TyInt())
    }
    it("Simple function Int -> Int") {
      """ \x:Int.x """ should haveType(TyArrow(TyInt(), TyInt()))
    }
    it("Simple function Int -> Int applied") {
      """(\x:Int.x) 2""" should haveType(TyInt())
    }
    it("IF THEN ELSE") {
      """IF TRUE THEN 3 ELSE 3""" should haveType(TyInt())
    }
    it("Fold") {
      """FOLD [IntList]  < cons = { hd = 3, tl = nil  } > :
      < nil: {}, cons: { hd:Int, tl: IntList }>""" should haveType(TyVar("IntList"))
    }
    it("UNFOLD") {
      """UNFOLD [IntList] (FOLD [IntList] < nil = {} > 
         : < nil: {}, cons: { hd:Int, tl: IntList }>)""" should haveType(TyVariant(
        Map("nil" -> TyRecord(Map()), "cons" -> TyRecord(Map("hd" -> TyInt(), "tl" -> TyVar("IntList"))))))
    }
    it("Ascription") {
      """3 : Int""" should haveType(TyInt())
    }
    it("Selection") {
      """{a=3, b=4}!a""" should haveType(TyInt())
    }
    it("CASE OF") {
      """  \x:IntList. CASE unapp x OF
            < nil = x > => 0;
            < cons = x > => (x ! hd) 
    """ should haveType(TyArrow(TyVar("IntList"), TyInt()))
    }
  }
  describe("Test case 2: Type Application") {
    it("Type Application with Variable as Argument") {
      val Z = TyIndex(0, new ShowId("Z"))
      """
     \Z::*.\x:List Z.UNFOLD [List Z] x
    """ should haveType(
        TyForall(new ShowId("Z"),
          KiStar(),
          TyArrow(TyApp(TyVar("List"), Z),
            TyVariant(
              Map("nil" -> TyRecord(Map()), "cons" -> TyRecord(Map("hd" -> Z, "tl" -> TyApp(TyVar("List"), Z))))))))
    }
    
    it("Type Application with Variable as Argument2") {
      """
    \Z::*.\x: Pair Z.x
    """ should haveType(
        TyForall(new ShowId("Z"),
          KiStar(),
          TyArrow(
            TyAbs(new ShowId("Y"), KiStar(), TyRecord(Map(
              "x" -> TyIndex(1, new ShowId("Z")),
              "y" -> TyIndex(0, new ShowId("Y"))))),
            TyAbs(new ShowId("Y"), KiStar(), TyRecord(Map(
              "x" -> TyIndex(1, new ShowId("Z")),
              "y" -> TyIndex(0, new ShowId("Y"))))))))
    }
    it("Type Application with Variable as Argument3") {
      """
    \A::*.\B::*.\x:Pair A B.\Z::*.\f:Pair A B -> Z. f x
    """ should haveType(
        TyForall(new ShowId("A"), KiStar(),
          TyForall(new ShowId("B"), KiStar(),
            TyArrow(TyApp(TyApp(TyVar("Pair"), TyIndex(1, new ShowId("A"))), TyIndex(0, new ShowId("B"))),
              TyForall(new ShowId("Z"), KiStar(),
                TyArrow(
                  TyArrow(TyApp(TyApp(TyVar("Pair"), TyIndex(2, new ShowId("A"))), TyIndex(1, new ShowId("B"))),
                    TyIndex(0, new ShowId("Z"))),
                  TyIndex(0, new ShowId("Z"))))))))
    }
    it("Type partial Fix") {
      val Z = TyIndex(0, new ShowId("Z"))
      """
     (\A::*.\B::*. 
     (\x:Fix A B.  (UNFOLD[Fix A B] x) x ))
    """ should haveType("""
        FORALL A::*.FORALL B::*. MU (\R::*=>*. \args::*. R args -> A -> B, {}) -> A -> B """)
    }
    it("Type UNFOLD FIX A B") {
      """
     \A::*.\B::*.\x:Fix A B.  
      UNFOLD [Fix A B ] x
      """ should haveType(
        """FORALL A::*.FORALL B::*.
        MU (\R::*=>*. \args::*. R args -> A -> B, {}) -> MU (\R::*=>*. \args::*. R args -> A -> B, {}) -> A -> B""")
    }
     
  }
}
