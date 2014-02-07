
//TODO amb: 
//BINDING gen  : (List, ID) FOR ListC = concatMap2 SYNTAX VAR <- BINDEE 
//BINDING gen2 : (List, ID) FOR ListC = concatMap2 SYNTAX BINDEE -> VAR 

import de.tuberlin.uebb.smith.modules._
import de.tuberlin.uebb.smith.modules.Syntax._
import scala.collection.immutable.ListMap
import org.scalatest.matchers._
import org.scalatest.{ FunSpec, Inside, BeforeAndAfter }
import scala.language.implicitConversions
import scalaz._
import Scalaz._

class AdaptiveParserTest extends FunSpec with Inside with ShouldMatchers with SDFGenerator
    with Driver with TypeChecker with DependenceAnalysis with ContextChecker with AdaptiveParser with BeforeAndAfter {


  after{ 
    initParser()
    clearSDF()
  }

  object Parser {
    def parseTestAsVal(input: String) = {
      for (
        term <- parseSDF("VAL x = " + input, "DefRest");
        r <- parseDefRest(term,Position(0,0))
      ) yield r._1.asInstanceOf[ValDef].e
    }

    def parseTestAsType(input: String) = {
      for (
        term <- parseSDF("TYPE x = " + input, "DefRest");
        r <- parseDefRest(term,Position(0,0))
      ) yield r._1.asInstanceOf[TypeDef].t
    }
    def parseTestAs[A](input: String) = {
      for (
        term <- parseSDF(input, "DefRest");
        r <- parseDefRest(term,Position(0,0))
      ) yield r._1.asInstanceOf[A]
    }

    def parseTestDefRest(input: String) = {
      for (
        term <- parseSDF(input, "DefRest");
        r <- parseDefRest(term,Position(0,0))
      ) yield r
    }

    def parseTestAsModuleHeader(input: String) = {
      for (
        term <- parseSDF(input, "ModuleRest");
        r <- parseModuleHeader(term)
      ) yield r.asInstanceOf[ModuleHeader]
    }
    def parseTest(input: String) = {
      val res = for (
        term <- parseSDF(input, "DefRest");
        r <- parseDefRest(term,Position(0,0));
        x <- true.successNel[Error]
      ) yield x 
      res
    }


  }

  class ParseMatcher[T](expected: T, parser: String => ValidationNEL[Error, T]) extends Matcher[String] {
    def apply(left: String) = {
      val result = parser(left)
      val failureMessageSuffix = result match {
        case Failure(e) => "Error: " + e.list.reduce(_ & _)
        case Success(e) => "'" + left + "' was parsed as " + e + " but expected was " + expected.toString
      }
      val negatedFailureMessageSuffix = "TODO negatedFailureMessageSuffix ParseMatcher"
      val equals = result.fold[Boolean]((_ => false), _ == expected)
      MatchResult(
        equals,
        failureMessageSuffix,
        negatedFailureMessageSuffix,
        failureMessageSuffix,
        negatedFailureMessageSuffix)
    }
  }

  def parseAsValueWith(w:String ,e: Expr) = {
    
    var context = Context(SymbolTable(Map(), Map()), Map(), Map(), ListMap())
    var dic = Dictionary(Map(),Map()) 
    var wrest = w
    var b = true
    while (wrest != "" && b) {
    val foo = for {
        (d1, rest, offset) <- parseSDF(wrest, "DefRest").flatMap(parseDefRest(_,Position(0,0)))
        (d2, ctx2) <- handleDef(d1,dic, "test", context);
        (dic2) <- dic.addDef(d2, "test");
        _ <- handleSyntaxExtention(d2, dic2, "test", ctx2)
    } yield {
      wrest = rest
      context = ctx2
      dic = dic2
    }
      b = foo.isSuccess
    }
    new ParseMatcher(e, Parser.parseTestAsVal)
  }
  def parseAsValue(e: Expr) = new ParseMatcher(e, Parser.parseTestAsVal)
  def parseAsType(t: Type) = new ParseMatcher(t, Parser.parseTestAsType)
  def parseAsModuleHeader(m: ModuleHeader) = new ParseMatcher(m, Parser.parseTestAsModuleHeader)
  def parseAsDefRest(d: (Def, String)) = new ParseMatcher(d, Parser.parseTestDefRest)
  def parseAs[A](d:A) = new ParseMatcher(d, Parser.parseTestAs[A])
  def beValid() = new ParseMatcher(true, Parser.parseTest)

  describe("Test case 1: Basic values") {
    
    it("Should parse binary op") {
    "3 + 2" should parseAsValueWith("""VAL plus = \x:Int.\y:Int. ADD x y SYNTAX ARG + ARG""", App(App(Var("plus"),Num(3)),Num(2)) )
    }
    it("Should parse binary op left") {
    "3 + 2 + 4" should parseAsValueWith("""VAL plus = \x:Int.\y:Int. ADD x y SYNTAX  ARG + ARG
                                           BRACKET plusplust = ARG + ARG + ARG AS ( ARG +  ARG ) + ARG 
""",  App(App(Var("plus"),App(App(Var("plus"),Num(3)),Num(2))),Num(4)))
    }
    it("Should parse binary ops with brackets") {
    "3 + 2 * 4" should parseAsValueWith("""VAL plus = \x:Int.\y:Int. ADD x y SYNTAX  ARG + ARG
                                           VAL mal = \x:Int.\y:Int. ADD x y SYNTAX  ARG * ARG
                                           BRACKET plusMal = ARG + ARG * ARG AS ARG + ( ARG * ARG )
                                            """,  App(App(Var("plus"),Num(3)),App(App(Var("mal"),Num(2)),Num(4)) ))
    }
    it("Should parse binary ops with mul higher than +") {
    "MUL 2 4 + 3 " should parseAsValueWith("""VAL plus = \x:Int.\y:Int. ADD x y SYNTAX  ARG + ARG
                                           BRACKET plusMal = MUL ARG ARG + ARG AS << MUL ARG ARG >> + ARG USING << >>
                                            """,  App(App(Var("plus"),Mul(Num(2),Num(4))),Num(3)))
    }
    it("Should parse binary ops with mul lower than +") {
    "MUL 2 4 + 3 " should parseAsValueWith("""VAL plus = \x:Int.\y:Int. ADD x y SYNTAX ARG + ARG
                                           BRACKET plusMal = MUL ARG ARG + ARG AS  MUL ARG < ARG  + ARG > USING < >
                                            """,  Mul(Num(2),App(App(Var("plus"),Num(4)),Num(3))))
    }
    it("Should work with simple transitivity in brackets") {
    "3 - 2 * 4" should parseAsValueWith("""VAL plus = \x:Int.\y:Int. ADD x y SYNTAX ARG + ARG
                                           VAL mal = \x:Int.\y:Int. ADD x y SYNTAX ARG * ARG
                                           VAL minus = \x:Int.\y:Int. SUB x y SYNTAX ARG - ARG
                                           BRACKET plusMal =   ARG AS   ARG + ( ARG  * ARG )
                                           BRACKET plusMinus=   ARG AS   ARG - ( ARG  + ARG )
                                            """,  App(App(Var("minus"),Num(3)),App(App(Var("mal"),Num(2)),Num(4)) ))
    }
    it("Should parse overloaded syntax") {
      val st =  App(App(Var("plusS"),Num(3)),Num(2))
      val nu =  App(App(Var("plusN"),Num(3)),Num(2))
      "3 + 2" should parseAsValueWith("""VAL plusN = \x:Int.\y:Int. ADD  x y SYNTAX ARG + ARG
                                       VAL plusS = \x:String.\y:String. CONCAT  x y SYNTAX ARG + ARG
                                            """,  ExprAmb(st,nu))
    }
    it("Should parse where with bracket") {
      val nu =  App(App(Var("plusN"),Var("a")),
        LetPar("where",List(
          BindingN("assign", "a", Num(3)),
          BindingN("assign", "b", Num(5))
        ),
        Var("b")))
     """a + b
        where
        a = 3;
        b = 5;
      """ should parseAsValueWith("""
        PUBLIC TYPE ID = \X::*.X
        PAR BINDER where : (ID,ID) =  \A::*.\x:A.x 
            SYNTAX BODY where BINDINGS 

        VAL bid = \A::*.\B::*.\x:A. x 
        BINDING assign : (ID, ID) FOR where = bid SYNTAX VAR = BINDEE;
        VAL plusN = \x:Int.\y:Int. ADD  x y SYNTAX ARG + ARG
        BRACKET wp = ARG + ARG where ARG AS  ARG + ( ARG where ARG )
        VAL a = 33
                                         """, nu )
    }
  }
}
