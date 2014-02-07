import de.tuberlin.uebb.smith.modules._
import de.tuberlin.uebb.smith.modules.Syntax._
import org.scalatest.matchers._
import org.scalatest.{ FunSpec, Inside, BeforeAndAfter }
import scala.language.implicitConversions
import scalaz._
import Scalaz._

class ParserTest extends FunSpec with Inside with ShouldMatchers with Parser 
 with BeforeAndAfter{
  this: Parser =>

  before{ 
    initParser()
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
        (t,r,p) <- parseDefRest(term,Position(0,0))
      ) yield (t,r)
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

  def parseAsValue(e: Expr) = new ParseMatcher(e, Parser.parseTestAsVal)
  def parseAsType(t: Type) = new ParseMatcher(t, Parser.parseTestAsType)
  def parseAsModuleHeader(m: ModuleHeader) = new ParseMatcher(m, Parser.parseTestAsModuleHeader)
  def parseAsDefRest(d: (Def, String)) = new ParseMatcher(d, Parser.parseTestDefRest)
  def parseAs[A](d:A) = new ParseMatcher(d, Parser.parseTestAs[A])
  def beValid() = new ParseMatcher(true, Parser.parseTest)

  describe("Test case 1: Basic values") {
    it("Should parse a number") {
      "23" should parseAsValue(Num(23))
    }
    it("Should parse a negative number") {
      "-23" should parseAsValue(Num(-23))
    }
    it("Should parse a negative number with whitespace") {
      "-23" should parseAsValue(Num(-23))
    }
    it("Should parse variables") {
      "x" should parseAsValue(Var("x"))
    }
    it("Should parse variables with parentheses") {
      "(x)" should parseAsValue(Var("x"))
    }
    it("Should parse multi-letter variables") {
      "xzy" should parseAsValue(Var("xzy"))
    }

    it("Should parse variable starting with keyword") {
      "TRUExzy" should parseAsValue(Var("TRUExzy"))
    }
    it("Should parse keyword with parentheses") {
      "(TRUE)" should parseAsValue(Bool(true))
    }
    it("Should parse string") {
      "\"Foo\"" should parseAsValue(Str("Foo"))
    }
    it("Should parse a number with comment") {
      "23 --comment" should parseAsValue(Num(23))
    }
    it("Should parse a simple lambda expressoion") {
      "\\a : Int . a" should parseAsValue(Abs("a", TyInt(), Var("a")))
    }
    it("Should parse a simple application") {
      "x x" should parseAsValue(App(Var("x"), Var("x")))
    }
    it("Should parse true") {
      "TRUE" should parseAsValue(Bool(true))
    }
    it("Should parse false") {
      "FALSE" should parseAsValue(Bool(false))
    }
    it("Should parse if then else") {
      "IF TRUE THEN 3 ELSE 2" should parseAsValue(Cond(Bool(true),Num(3), Num(2)))
    }
    it("Should parse a type abstraction") {
      "\\ a:: * . a" should parseAsValue(TAbs("a",KiStar(),Var("a")))
    }
    it("Should parse a abstraction") {
      "\\ a: Int . a" should parseAsValue(Abs("a",TyInt(),Var("a")))
    }
    it("Should parse a application with abstraction as argument with prackets") {
      """a (\a:Int . a)""" should parseAsValue(App(Var("a"), Abs("a",TyInt(),Var("a"))))
    }
    it("Should parse a application with abstraction as argument") {
      """a \a:Int . a""" should parseAsValue(App(Var("a"), Abs("a",TyInt(),Var("a"))))
    }
    it("Should parse if then else with application") {
      "IF TRUE THEN 3 ELSE f 2" should parseAsValue(Cond(Bool(true),Num(3), App(Var("f"),Num(2))))
    }
    it("Should parse application with if then else in arg") {
      "f IF TRUE THEN 3 ELSE 2" should parseAsValue(App(Var("f"),Cond(Bool(true),Num(3), Num(2))))
    }

    it("Should parse a type application") {
      "a [Int]" should parseAsValue(TApp(Var("a"), TyInt()))
    }
    it("Should parse a fold") {
      "FOLD [Int] a" should parseAsValue(Fold(TyInt(), Var("a")))
    }
    it("Should parse a unfold") {
      "UNFOLD [Int] a" should parseAsValue(Unfold(TyInt(), Var("a")))
    }

    it("Should parse a record") {
      "{a=1, b=2}" should parseAsValue(Record(Map(("a",Num(1)),("b",Num(2)))))
    }
    it("Should parse a selection") {
      "a ! b" should parseAsValue(Selection(Var("a"),"b"))
    }
    it("Should parse a variant") {
      "<a=3> : Int" should parseAsValue(Variant("a",Num(3),TyInt()))
    }
    it("Should parse an ascription") {
      "a : Int" should parseAsValue(Ascription(Var("a"),TyInt()))
    }
    it("Should parse case of") {
      "CASE a OF < b = c> => 3; <d = f> => 4" should parseAsValue(CaseOf(Var("a"),List(
        Branch("b","c",Num(3)),
        Branch("d","f",Num(4))
      )))
    }
    it("Should parse a seq binding") {
      "LET SEQ < a > b < c > = 3 d < f > : Int = 4 IN x" should parseAsValue(LetSeq("a",List(
        BindingN("b", "c", Num(3)),
        BindingAs("d", "f", TyInt(), Num(4))
      ), Var("x")))
    }
    it("Should parse a par binding") {
      "LET PAR < a > b < c > = 3 d < f > : Int = 4 IN x" should parseAsValue(LetPar("a",List(
        BindingN("b", "c", Num(3)),
        BindingAs("d", "f", TyInt(), Num(4))
      ), Var("x")))
    }

    it("Should parse appsyntax ") {
      """VAL f = \x:Int.\y:Int.x SYNTAX ARG ! ARG""" should parseAs[ValDef](ValDef(Private,"f",
      Abs("x",TyInt(),Abs("y",TyInt(),Var("x"))),Some(List(AppArg, "!",AppArg))))
    }
    it("Should parse ADD") {
      "ADD x y" should parseAsValue(Add(Var("x"),Var("y")))
    }
    it("Should parse SUB") {
      "SUB x y" should parseAsValue(Sub(Var("x"),Var("y")))
    }
    it("Should parse MUL") {
      "MUL x y" should parseAsValue(Mul(Var("x"),Var("y")))
    }
    it("Should parse DIV") {
      "DIV x y" should parseAsValue(Div(Var("x"),Var("y")))
    }
    it("Should parse GT") {
      "GT x y" should parseAsValue(Greater(Var("x"),Var("y")))
    }
    it("Should parse CONCAT") {
      "CONCAT x y" should parseAsValue(Concat(Var("x"),Var("y")))
    }
    it("Should parse CHAROF") {
      "CHAROF x y" should parseAsValue(CharOf(Var("x"),Var("y")))
    }
    it("Should parse SIZE") {
      "SIZE x" should parseAsValue(StrSize(Var("x")))
    }

  }
  describe("Test case 2: Module header") {
    it("Should parse simple module header ") {
      "MODULE test" should parseAsModuleHeader(ModuleHeader("test": String, Nil: List[Import], "": String)(Position(0,0)))
    }
    it("Should parse simple module header with rest") {
      "MODULE test VAL x = x" should parseAsModuleHeader(ModuleHeader("test": String, Nil: List[Import], "VAL x = x": String)(Position(0,0)))
    }
    it("Should parse simple module header with rest containing seq binder") {
      "MODULE test SEQ BINDER foo (id,id) = x" should parseAsModuleHeader(
        ModuleHeader("test": String, Nil: List[Import], "SEQ BINDER foo (id,id) = x": String)(Position(0,0)))
    }

    it("Should parse simple module header with imports") {
      """MODULE test
         PUBLIC IMPORT X
         PRIVATE IMPORT Y
         IMPORT Z""" should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, NotQualified("X"),Nil), Import(Private, NotQualified("Y"),Nil), Import(Private, NotQualified("Z"),Nil)): List[Import], "": String)(Position(0,0)))
    }

    it("Should parse simple module header with qualified imports") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X
         PRIVATE IMPORT Y
         IMPORT QUALIFIED Z AS ZZ""" should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),Nil), Import(Private, NotQualified("Y"),Nil), Import(Private, QualifiedAs("Z", "ZZ"),Nil)): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with only modification") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X ONLY foo 
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(Only(List(ID("foo")))))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with multiple only modification") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X ONLY foo ONLY bar ONLY foobar
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(
             Only(List(ID("foo"))),
             Only(List(ID("bar"))),
             Only(List(ID("foobar")))
           ))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with only modification with multiple identifiers") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X ONLY foo bar foobar
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(Only(List(
             ID("foo"),
             ID("bar"),
             ID("foobar")
           ))))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with without modification") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X WITHOUT foo 
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(Without(List(ID("foo")))))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with multiple without modification") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X WITHOUT foo WITHOUT bar WITHOUT foobar
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(
             Without(List(ID("foo"))),
             Without(List(ID("bar"))),
             Without(List(ID("foobar")))
           ))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with without modification with multiple identifiers") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X WITHOUT foo bar foobar
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(Without(List(
             ID("foo"),
             ID("bar"),
             ID("foobar")
           ))))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with renaming modification") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X RENAMING (foo AS bar) 
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(ImportRenaming(List(
             IdentifierRenaming("foo","bar")
           ))))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with multiple renaming modification") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X RENAMING (foo AS bar) RENAMING (bar AS foo)
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(
             ImportRenaming(List(IdentifierRenaming("foo","bar"))),
             ImportRenaming(List(IdentifierRenaming("bar","foo")))
           ))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with one renaming modification with multiple renaimings") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X RENAMING (foo bar) AS (bar foo)
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(
             ImportRenaming(List(
               IdentifierRenaming("foo","bar"),
               IdentifierRenaming("bar","foo")
             ))))): List[Import], "": String)(Position(0,0)))
    }

    it("Should parse module header with mix modification") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X RENAMING (foo bar) AS (bar foo) ONLY foo bar WITHOUT foobar
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(
             ImportRenaming(List(
               IdentifierRenaming("foo","bar"),
               IdentifierRenaming("bar","foo"))),
             Only(List(ID("foo"),ID("bar"))),
             Without(List(ID("foobar")))
           ))): List[Import], "": String)(Position(0,0)))
    }
    it("Should parse module header with only modification with renaming") {
      """MODULE test
         PUBLIC IMPORT QUALIFIED X  ONLY (foo bar) AS (bar foo) (a AS b)
         """ should parseAsModuleHeader(ModuleHeader("test": String,
           List(Import(Public, Qualified("X"),List(Only(List(
             IdentifierRenaming("foo","bar"),
             IdentifierRenaming("bar","foo"),
             IdentifierRenaming("a","b")
           ))))): List[Import], "": String)(Position(0,0)))
    }
  }

  describe("Test case 3: DefRest") {
    it("Should parse simple DefRest ") {
      "VAL x = 1\nVAL y = 1\nVAL z = 1\nVAL t = 1" should parseAsDefRest(
        (ValDef(Private, "x", Num(1)), "VAL y = 1\nVAL z = 1\nVAL t = 1"))
    }
    it("Should parse simple DefRest with private ") {
      "PRIVATE VAL x = 1\nPRIVATE VAL y = 1\nVAL z = 1\nVAL t = 1" should parseAsDefRest(
        (ValDef(Private, "x", Num(1)), "PRIVATE VAL y = 1\nVAL z = 1\nVAL t = 1"))
    }
    it("Should parse simple DefRest with public ") {
      "PUBLIC VAL x = 1\nPUBLIC VAL y = 1\nVAL z = 1\nVAL t = 1" should parseAsDefRest(
        (ValDef(Public, "x", Num(1)), "PUBLIC VAL y = 1\nVAL z = 1\nVAL t = 1"))
    }
    it("Should parse simple DefRest with comment before rest") {
      "VAL x = 1\n--VAL y = 1\nVAL z = 1\nVAL t = 1" should parseAsDefRest(
        (ValDef(Private, "x", Num(1)), "VAL z = 1\nVAL t = 1"))
    }
    it("Should parse simple DefRest with multiply comments before rest") {
      "VAL x = 1\n--VAL y = 1\n--VAL z = 1\nVAL t = 1" should parseAsDefRest(
        (ValDef(Private, "x", Num(1)), "VAL t = 1"))
    }
    it("Should parse simple DefRest with whitespace and comment before rest") {
      "VAL x = 1\n  --VAL y = 1\nVAL z = 1\nVAL t = 1" should parseAsDefRest(
        (ValDef(Private, "x", Num(1)), "VAL z = 1\nVAL t = 1"))
    }
    it("Should parse simple DefRest with comment in rest") {
      "VAL x = 1\nVAL y = 1\n--VAL z = 1\nVAL t = 1" should parseAsDefRest(
        (ValDef(Private, "x", Num(1)), "VAL y = 1\n--VAL z = 1\nVAL t = 1"))
    }
    it("Should parse simple DefRest with empty comment in rest") {
      "VAL x = 1\nVAL y = 1\nVAL z = 1--\nVAL t = 1" should parseAsDefRest(
        (ValDef(Private, "x", Num(1)), "VAL y = 1\nVAL z = 1--\nVAL t = 1"))
    }
    it("Should parse simple DefRest with comment at the end of rest") {
      "VAL x = 1\nVAL y = 1\nVAL z = 1\n--VAL t = 1" should parseAsDefRest(
        (ValDef(Private, "x", Num(1)), "VAL y = 1\nVAL z = 1\n--VAL t = 1"))
    }
  }
  describe("Test case 4: Types") {
    it("Should parse type Int") {
      "Int" should parseAsType(TyInt())
    }
    it("Should parse type String") {
      "String" should parseAsType(TyString())
    }
    it("Should parse type Bool") {
      "Bool" should parseAsType(TyBool())
    }
    it("Should parse type var") {
      "a" should parseAsType(TyVar("a"))
    }
    it("Should parse type var with parenthesis") {
      "(a)" should parseAsType(TyVar("a"))
    }
    it("Should parse type qualified var") {
      "M.a" should parseAsType(TyQVar("M.a"))
    }
    it("Should parse type arrow") {
      "Bool -> Int" should parseAsType(TyArrow(TyBool(),TyInt()))
    }
    it("Should parse multiple type arrow") {
      "Bool -> Int -> String" should parseAsType(TyArrow(TyBool(),TyArrow(TyInt(),TyString())))
    }
    it("Should parse type application") {
      "fun Int" should parseAsType(TyApp(TyVar("fun"),TyInt()))
    }
    it("Should parse multiple type application") {
      "g f Int" should parseAsType(TyApp(TyApp(TyVar("g"),TyVar("f")),TyInt()))
    }
    it("Should parse type abstraction with star") {
      "\\ x :: * . Int" should parseAsType(TyAbs("x",KiStar(),TyInt()))
    }
    it("Should parse forall with kind arrow") {
      "FORALL x :: * => * . Int" should parseAsType(TyForall("x",KiArrow(KiStar(),KiStar()),TyInt()))
    }
    it("Should parse appsyntax") {
      """TYPE f = FORALL x :: * => * . Int SYNTAX ARG ! ARG""" should parseAs[TypeDef](TypeDef(Private,"f",
        TyForall("x",KiArrow(KiStar(),KiStar()),TyInt())
        ,Some( List(AppArg, "!",AppArg))))
    }
    it("Should parse forall with kind arrow and parenthesis") {
      "FORALL x :: (* => *) . Int" should parseAsType(TyForall("x",KiArrow(KiStar(),KiStar()),TyInt()))
    }
    it("Should parse a pair") {
      "Int ** Int" should parseAsType(TyPair(TyInt(),TyInt()))
    }
    it("Should parse a pair selection first") {
      "(Int ** Int).1" should parseAsType(TyFst(TyPair(TyInt(),TyInt())))
    }
    it("Should parse a pair selection second") {
      "(Int ** Int).2" should parseAsType(TySnd(TyPair(TyInt(),TyInt())))
    }
    it("Should parse forall with star") {
      "FORALL x :: * . Int" should parseAsType(TyForall("x",KiStar(),TyInt()))
    }
    it("Should parse record") {
      "{x:Int, y:Bool}" should parseAsType(TyRecord(Map(("x",TyInt()),("y",TyBool()))))
    }
    it("Should parse variant") {
      "<x:Int, y:Bool>" should parseAsType(TyVariant(Map(("x",TyInt()),("y",TyBool()))))
    }
    it("Should parse MU") {
      "MU (Bool,Bool)" should parseAsType(TyMu(TyBool(),TyBool()))
    }

  }
  describe("Test case 5: Binder") {
    it("Should parse simple seq binder") {
      "SEQ BINDER a : (Int , Int) = 23" should parseAs[BinderDef](BinderDef(Private,Seq,"a",TyInt(),TyInt(),Num(23),None))
    }
    it("Should parse simple par binder") {
      "PAR BINDER a : (Int , Int) = 23" should parseAs[BinderDef](BinderDef(Private,Par,"a",TyInt(),TyInt(), Num(23),None))
    }
    it("Should parse seq binder with lr bindings") {
      """SEQ BINDER do : (Int , Int) = id
         SYNTAX do { LR BINDINGS BODY }""" should parseAs[BinderDef](BinderDef(Private,Seq,"do",TyInt(),TyInt(),Var("id"),Some(
         List("do","{", LRBindings, BinderBody, "}" ))))
    }

    it("Should parse seq binder with rl bindings") {
      """SEQ BINDER do : (Int , Int) = id
         SYNTAX do { RL BINDINGS BODY }""" should parseAs[BinderDef](BinderDef(Private,Seq,"do",TyInt(),TyInt(),Var("id"),Some(
         List("do","{", RLBindings, BinderBody, "}" ))))
    }
    it("Should parse seq binder with bindings") {
      """SEQ BINDER do : (Int , Int)  = id
         SYNTAX do { BINDINGS BODY }""" should parseAs[BinderDef](BinderDef(Private,Seq,"do",TyInt(),TyInt(),Var("id"),Some(
         List("do","{", Bindings, BinderBody, "}" ))))
    }
  }
  describe("Test case 6: Binding") {
    it("Should parse arrow binding ") {
      """BINDING arrow : (Int , Int)  FOR do = bind
         SYNTAX VAR <- BINDEE ;""" should parseAs[BindingDef](BindingDef(Private,"arrow",TyInt(),TyInt(),"do",Var("bind"),Some(
         List(BindingVar, "<-", BindingBindee, ";"))))
    }
    it("Should parse eq binding") {
      """BINDING eq :(Int , Int)  FOR do = id
         SYNTAX VAR = BINDEE ;""" should parseAs[BindingDef](BindingDef(Private,"eq",TyInt(),TyInt(),"do",Var("id"),Some(
         List(BindingVar, "=", BindingBindee, ";"))))
    }
    it("Should parse amb binding") {
      """BINDING eq : (Int , Int) FOR do = id
         SYNTAX VAR = BINDEE;""" should parseAs[BindingDef](BindingDef(Private,"eq",TyInt(),TyInt(),"do",Var("id"),Some(
         List(BindingVar, "=", BindingBindee, ";"))))
    }
  }
  describe("Test case 6: Brackets") {
    it("Should parse simple brackets") {
      """BRACKET pfs = ARG + ARG * ARG AS ARG + ( ARG * ARG )""" should parseAs[BracketDef](BracketDef(Private,
      "pfs",BracketLhs(List(AppArg, "+", AppArg,"*", AppArg)),
        BracketRhs(List(AppArg, "+", "(",AppArg,"*", AppArg,")"))
      ))
    }
    it("Should parse brackets with using") {
      """BRACKET pfs = ARG + ARG * ARG AS ARG + [ ARG * ARG ] USING [ ]""" should parseAs[BracketDef](BracketDef(Private,
      "pfs",BracketLhs(List(AppArg, "+", AppArg,"*", AppArg)),
        BracketRhs(List(AppArg, "+", "[",AppArg,"*", AppArg,"]"),Some(BracketUsing("[","]")))
      ))
    }
  }
  describe("Test case 7: amb") {
    it("Should parse without amb") {
      """VAL x = 2""" should beValid
    }
    it("Should parse without amb 1") {
      """VAL lift = \X::*.\x:X. M [X]""" should beValid
    }
    it("Should parse without amb 2") {
      """
        VAL main =  LET SEQ <do> 
        mlet < z > = {c = 3, x = 4} 
        mlet < a > = lift [Int] 3
        IN   
        lift [Int] 3
        """ should beValid 
    }
    it("Should parse without amb 3") {
      """
        VAL main =  ADD (f 3) 2 
        """ should beValid
    }
    it("Should parse without amb 4") {
      """
        VAL fact = \fact:Int -> Int. \x:Int. IF (eq x 0) THEN 1 ELSE fact (SUB x 1)
        """ should beValid
    }
    it("Should parse without amb 5") {
      """
        VAL tail = \A::*. \xs:List A.
            CASE UNFOLD [List A] xs OF
            <cons=cons> => cons.tl
        """ should beValid
    }
    it("Should parse without amb 6") {
      """
    VAL concat1 = \A::*. \concat: List A -> List A -> List A.
    \xs:List A. \ys:List B.
        IF isNil [A] THEN ys
        ELSE
        Cons (head [A] xs) (concat (tail [A] xs) ys)
        """ should beValid
    }
    it("Should parse case of ending with selection ") {
      "VAL X = CASE a OF < b = c> => 3; <d = f> => f ! a" should beValid
    }
    it("Should parse type application ") {
    """VAL concat = \A::*.
       Fix [List A] [List A->List A] (concat1 [A])""" should beValid
    }

    it("Should parse without amb 7") {
    """VAL concatMap1 = \A::*. \B::*.
       \concatMap:(A->List B)->List A->List B.
       \f:A->List B. \xs:List A.
         IF isNil [A] xs THEN
          Nil [A]
         ELSE
          concat [B] (f (head [A] xs)) (concatMap f (tail [A] xs))""" should beValid
    }
     
    it("Should parse without amb 8") {
      """VAL concatMap = \A::*. \B::*.
      Fix [A->List B] [List A->List B]
      (concatMap1 [A] [B])""" should beValid
    }

  }
}
