package de.tuberlin.uebb.smith.modules
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.jsglr.client.Asfix2TreeBuilder
import org.spoofax.jsglr.client.ParseTable
import org.spoofax.jsglr.client.imploder.TermTreeFactory
import org.spoofax.jsglr.client.imploder.TreeBuilder
import org.spoofax.jsglr.io.SGLR
import org.spoofax.jsglr.shared.Tools
import org.spoofax.terms.TermFactory
import org.spoofax.terms.io.binary.TermReader
import de.tuberlin.uebb.smith.modules._
import org.spoofax.terms.StrategoAppl
import org.spoofax.terms.StrategoList
import org.spoofax.terms.StrategoString
import org.spoofax.jsglr.client.imploder._
import org.spoofax.jsglr.client._
import org.spoofax.jsglr.shared._
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scalaz._
import Scalaz._
import scala.reflect.runtime.universe._
import Syntax._
import TypeSynonyms._

trait Parser {

  type Parser[A] = IStrategoTerm => Result[A]
  type Converter[A] = Function1[Array[IStrategoTerm], Result[A]]

  var parseTableFile = "./SDF2/BinderLang.tbl" 

  var modules: Map[String, Module] = Map()

  def parseSDFDebug(input: String, start: String, ambiguityIsError: Boolean = false, filter: Boolean = true): IStrategoTerm =
    {
      val logging = false
      val detectCycles = true
      val recover = false
      val heuristicFilters = true

      val factory = new TermFactory()

      val tableTerm = new TermReader(factory).parseFromFile(parseTableFile)
      val pt = new ParseTable(tableTerm, factory)
      val sglr = new SGLR(new Asfix2TreeBuilder(), pt)
      sglr.setUseStructureRecovery(recover)
      Tools.setLogging(logging)
      sglr.getDisambiguator.setFilterCycles(detectCycles)
      sglr.getDisambiguator.setFilterAny(filter)
      sglr.getDisambiguator.setHeuristicFilters(heuristicFilters)
      sglr.getDisambiguator.setAmbiguityIsError(ambiguityIsError)
      sglr.parse(input, null, start).asInstanceOf[IStrategoTerm]
    }

  def parseSDF(input: String, start: String, offset: Position = Position(0,0), fileName : String = "Unknown"): Result[IStrategoTerm] =
    {
      val logging = false
      val detectCycles = true
      val filter = true
      val recover = false
      val heuristicFilters = true

      val ambiguityIsError = false

      val factory = new TermFactory()

      val tableTerm = new TermReader(factory).parseFromFile(parseTableFile)
      val pt = new ParseTable(tableTerm, factory)
      val sglr = new SGLR(new Asfix2TreeBuilder(), pt)
      sglr.setUseStructureRecovery(recover)
      Tools.setLogging(logging)
      sglr.getDisambiguator.setFilterCycles(detectCycles)
      sglr.getDisambiguator.setFilterAny(filter)
      sglr.getDisambiguator.setHeuristicFilters(heuristicFilters)
      sglr.getDisambiguator.setAmbiguityIsError(ambiguityIsError)
      val tb = new TreeBuilder(new TermTreeFactory(new TermFactory()), false)
      sglr.setTreeBuilder(tb)
      try {
        sglr.parse(input, null, start).asInstanceOf[IStrategoTerm].successNel[Error]
      } catch {
        case e: BadTokenException =>
          //the exceptions starts counting lines with 1
          //the rest of jsglr, and by extension we, start with 0
          val column = e.getColumnNumber + offset.column - 1
          val line = e.getLineNumber + offset.line - 1
          val pos = Position(line, column)
          ParseError("Could not parse " + input + " with " + start + "\n" + e.getShortMessage,
                     PosAttribute(pos,pos, fileName)).failNel[IStrategoTerm]
        case e : StartSymbolException => 
          ParseError("File does not start with MODULE",
                     FileAttribute(fileName)).failNel[IStrategoTerm]
          
      }
    }

  def buildAttribute(d: IStrategoTerm): PosAttribute = {
    val att = d.getAttachment(null).asInstanceOf[ImploderAttachment]
    val startLine = att.getLeftToken.getLine + currentOffset.line  //they start counting at zero 
    val startColumn = att.getLeftToken.getColumn + currentOffset.column
    // is -1 if end of file
    val rEndCol = att.getRightToken.getEndColumn
    val endLine = att.getRightToken.getLine + currentOffset.line  
    val endColumn = if (rEndCol == -1) currentOffset.column
    else rEndCol + currentOffset.column

    val a = PosAttribute(Position(startLine, startColumn),
      Position(endLine, endColumn),
      currentInputFile)
    a
  }


  def initParser(): Unit = {
    currentOffset = Position(0, 0)
  }

  implicit class RichStrategoTerm(term: IStrategoTerm) {

    val att = buildAttribute(term)
    
    def asString(): Result[String] = term match {
      case e: StrategoString => e.stringValue().successNel[Error]
      case e                 => ParseError("Could not cast " + e + " to String", att).failNel[String]
    }
    def method(arg: String) = {
      var x = List(1, 2, 3)
      x
    }

    def asExpr(): Result[Expr] = term.as[StrategoAppl].flatMap(parseExpr(_))
    def asKind(): Result[Kind] = term.as[StrategoAppl].flatMap(parseKind(_))
    def asType(): Result[Type] = term.as[StrategoAppl].flatMap(parseType(_))

    def unpackRest(): Result[String] = term.as[StrategoAppl].flatMap(x =>
      if (x.getConstructor.getName == "Some")
        x.getAllSubterms()(0) asString
      else "".successNel[Error])

    def mapOption[T](f: IStrategoTerm => Result[T]): Result[Option[T]] =
      term.as[StrategoAppl].flatMap(x =>
        if (x.getConstructor.getName == "Some")
          f(x.getAllSubterms()(0)).map(Some(_))
        else
          None.successNel[Error])

    def as[T: TypeTag]: Result[T] =
      if (reflect.runtime.currentMirror.reflect(term).symbol.toType <:< typeOf[T])
        term.asInstanceOf[T].successNel[Error]
      else
        ParseError("Could not cast " + term.getClass + " to " + typeOf[T] + ". " + term, att).failNel[T]

    def $[A](name: String, f: Converter[A]): Result[A] =
      term.as[StrategoAppl].flatMap(app =>
        if (app.getConstructor.getName == name)
          f(app.getAllSubterms)
        else
          ParseError("Unknown term:\n " + term + " expected " + name, att).failNel[A])

    def asStrategoList[A](parser: IStrategoTerm => Result[A]): Result[List[A]] =
      term.as[StrategoList].flatMap { list =>
        list.asScala.toList.map(parser).sequence[({ type l[a] = Result[a] })#l, A]
      }
  }

  def parseAsArray[A](a: Array[IStrategoTerm], parser: IStrategoTerm => Result[A]): Result[List[A]] =
    a.toList.map(parser).sequence[({ type l[a] = Result[a] })#l, A]

  def parseModuleHeader(term: IStrategoTerm, fileName: String = "Unknown"): Result[ModuleHeader] =
  {
    currentInputFile = fileName
    term.$("ModuleRest", subterms =>
      for (
        imports <- subterms(1).asStrategoList(parseImport);
        name <- subterms(0) asString;
        rest <- subterms(2) unpackRest
      ) yield ModuleHeader(name, imports, rest)(buildAttribute(subterms(2)).start, fileName))
  }

  def parseVisibility(term: IStrategoTerm): Result[Visibility] =
    term.mapOption[Visibility](x =>
      x.as[StrategoAppl].flatMap(app =>
        app.getConstructor().getName() match {
          case "Public"  => Public.successNel[Error]
          case "Private" => Private.successNel[Error]
          case x         => ParseError("Expected Visiblity Term, not: " + x: String, buildAttribute(term)).failNel[Visibility]
        })).map(_.getOrElse(Private))

  def parseImport(term: IStrategoTerm): Result[Import] =
    {
      val att = buildAttribute(term)
      term.$("Import", subterms =>
        for (
          visibility <- parseVisibility(subterms(0): IStrategoTerm);
          name <- parseImportedModule(subterms(1): IStrategoTerm);
          mod <- subterms(2).asStrategoList(parseImportModification)
        ) yield Import(visibility, name, mod, att))
    }

  def parseImportedModule(term: IStrategoTerm): Result[ImportedModule] =
    term.as[StrategoAppl].flatMap(app =>
      app.getConstructor().getName() match {
        case "NotQualified" => term.$("NotQualified", subterms => subterms(0).asString().map(NotQualified(_)))
        case "Qualified"    => term.$("Qualified", subterms => subterms(0).asString().map(Qualified(_)))
        case "QualifiedAs" => term.$("QualifiedAs",
          subterms => for (
            qid <- subterms(0) asString;
            as <- subterms(1) asString
          ) yield QualifiedAs(qid, as))
        case x => ParseError("Expected ImportedModule Term, not: " + x: String, buildAttribute(term)).failNel[ImportedModule]
      })

  def parseImportModification(term: IStrategoTerm): Result[ImportModification] =
    term.as[StrategoAppl].flatMap(app =>
      app.getConstructor().getName() match {
        case "Renaming" => parseRenaming(app)
        case "Only"     => parseOnly(app)
        case "Without"  => parseWithout(app)
        case x          => ParseError("Expected ImportModification Term, not: " + x: String, buildAttribute(term)).failNel[ImportModification]
      })

  def parseRenaming(term: IStrategoTerm): Result[ImportRenaming] =
    term.$("Renaming", subterms =>
      for (
        rens <- subterms(0).asStrategoList(parseIdentiferRenaiming)
      ) yield ImportRenaming(rens.flatten))

  def parseIdentiferRenaiming(term: IStrategoTerm): Result[List[IdentifierRenaming]] =
    term.as[StrategoAppl].flatMap(app =>
      app.getConstructor().getName() match {
        case "SingleRenaming" => term.$("SingleRenaming", c => for (c0 <- c(0).asString; c1 <- c(1).asString) yield List(IdentifierRenaming(c0, c1)))
        case "ListRenaming" => term.$("ListRenaming", c => for (c0 <- parseIdentifers(c(0)); c1 <- parseIdentifers(c(1)))
          yield if (c0.size != c1.size) 
            return ParseError("Number of arguments for renaming don't macht" + "\n" + c0 + "\n" + c1, buildAttribute(term)).failNel[List[IdentifierRenaming]]
        else return c0.zip(c1).map { case (a, b) => IdentifierRenaming(a, b) }.successNel[Error])
        case x => ParseError("Expected IdentifierRenaming Term, not: " + x: String, buildAttribute(term)).failNel[List[IdentifierRenaming]]
      })

  def parseIdentifers(term: IStrategoTerm): Result[List[Identifier]] = term.asStrategoList((x => x.asString().map(string2Id(_))))

  def parseOnly(term: IStrategoTerm): Result[Only] = term.$("Only", subterms =>
    parseAsArray(subterms, parseImporedIdentifers).map(x => Only(x.flatten)))

  def parseWithout(term: IStrategoTerm): Result[Without] = term.$("Without", subterms =>
    parseAsArray(subterms, parseIdentifers).map(x => Without(x.flatten)))

  def parseImporedIdentifers(term: IStrategoTerm): Result[List[ImportedIdentifier]] =
    term.as[StrategoList].flatMap { list =>
      val l = list.asScala.toList
      if (l(0).isInstanceOf[StrategoAppl])
        term.asStrategoList(parseIdentiferRenaiming).map(x => x.flatten)
      else
        parseIdentifers(term)
    }

  var currentOffset = Position(0, 0)
  var currentInputFile  = "" 
  def parseDefRest(term: IStrategoTerm, offset: Position, fileName: String = "Unknown"): Result[(Def, Rest, Position)] = {
    currentOffset = offset
    currentInputFile = fileName
    term.$("DefRest", subterms =>
            for (
              d <- parseDef(subterms(0));
              rest <- subterms(1) unpackRest
            ) yield (d, rest, buildAttribute(subterms(1)).start)
        )
  }

  def parseDef(term: IStrategoTerm) : Result[Def] = 
      term.as[StrategoAppl].flatMap(app =>
        app.getConstructor().getName() match {
          case "ValDef" => parseValDef(term)
          case "TypeDef" => parseTypeDef(term)
          case "BinderDef" => parseBinder(term)
          case "BindingDef" => parseBinding(term)
          case "BracketDef" => parseBracket(term)
          case "amb" => {
            val c = app.getAllSubterms()
            for (c0 <- c(0).asStrategoList(parseDef(_)))
                yield DefAmb(c0(0), c0(1))
          }
          case x => ParseError("Expected Def, not: " + app.toString: String, buildAttribute(term)).failNel[Def]
        })

  def parseValDef(term: IStrategoTerm): Result[ValDef] =
    {
      val att = buildAttribute(term)
      term.$("ValDef", subterms =>
        for (
          visibility <- parseVisibility(subterms(0): IStrategoTerm);
          name <- subterms(1).asString;
          e <- subterms(2).asExpr;
          syn <- subterms(3).mapOption(parseAppSyntax)
        ) yield ValDef(visibility, name, e, syn, att))
    }

  def parseAppSyntax(term: IStrategoTerm): Result[List[AppSyntaxElement]] = term.$("AppSyntax",
    c => 
    for {
      l <- c(0).asStrategoList(parseAppSyntaxElement)
    } yield  l)

  def parseTypeDef(term: IStrategoTerm): Result[TypeDef] =
    {
      val att = buildAttribute(term)
      term.$("TypeDef", subterms =>
        for (
          visibility <- parseVisibility(subterms(0): IStrategoTerm);
          name <- subterms(1).asString;
          e <- subterms(2).asType;
          syn <- subterms(3).mapOption(parseAppSyntax)
        ) yield TypeDef(visibility, name, e, syn, att))
    }

  def parseBracket(term: IStrategoTerm): Result[BracketDef] =
    {
      val att = buildAttribute(term)
      term.$("BracketDef", subterms =>
        for (
          visibility <- parseVisibility(subterms(0): IStrategoTerm);
          name <- subterms(1).asString;
          l <- parseBracketLhs(subterms(2));
          r <- parseBracketRhs(subterms(3))
        ) yield BracketDef(visibility, name, l, r, att))
    }

  def parseBracketLhs(term: IStrategoTerm): Result[BracketLhs] =
    term.$("BracketLhs", c => c(0).asStrategoList(parseAppSyntaxElement).map(BracketLhs(_)))

  def parseBracketRhs(term: IStrategoTerm): Result[BracketRhs] =
    term.$("BracketRhs", c => for (a <- c(0).asStrategoList(parseAppSyntaxElement); u <- c(1).mapOption(parseUsing)) yield BracketRhs(a, u))

  def parseUsing(term: IStrategoTerm): Result[BracketUsing] = term.$("BracketUsing", c =>
    for (c0 <- c(0).asString; c1 <- c(1).asString) yield BracketUsing(c0, c1))

  def parseAppSyntaxElement(term: IStrategoTerm): Result[AppSyntaxElement] = term.as[StrategoAppl].flatMap(
    { d =>
      val c = d.getAllSubterms()
      d.getConstructor().getName() match {
        case "AppArg"    => AppArg.successNel[Error]
        case "AppLexeme" => c(0).asString.map(Lexeme(_))
        case x           => ParseError("Expected AppSyntaxElement, not: " + x: String, buildAttribute(term)).failNel[AppSyntaxElement]
      }
    })

  def parseBinder(term: IStrategoTerm): Result[BinderDef] =
    {
      val att = buildAttribute(term)
      term.$("BinderDef", subterms =>
        for (
          visibility <- parseVisibility(subterms(0): IStrategoTerm);
          btype <- parseBinderType(subterms(1));
          name <- subterms(2).asString;
          tyA <- subterms(3).asType;
          tyB <- subterms(4).asType;
          a <- subterms(5).as[StrategoAppl];
          e <- parseExpr(a);
          l <- subterms(6).mapOption(_.$("BinderSyntax", c => c(0).asStrategoList(parseBinderSyntaxElement)))
        ) yield BinderDef(visibility, btype, name, tyA, tyB, e, l, att))
    }

  def parseBinderType(term: IStrategoTerm): Result[BinderType] = term.as[StrategoAppl].flatMap(
    { d =>
      d.getConstructor().getName() match {
        case "SeqBinder" => Seq.successNel[Error]
        case "ParBinder" => Par.successNel[Error]
        case x           => ParseError("Expected SEQ or PAR, not: " + x: String, buildAttribute(term)).failNel[BinderType]
      }
    })
  def parseBinderSyntaxElement(term: IStrategoTerm): Result[BinderSyntaxElement] = term.as[StrategoAppl].flatMap(
    { d =>
      val c = d.getAllSubterms()
      d.getConstructor().getName() match {
        case "BinderBody"   => BinderBody.successNel[Error]
        case "LRBindings"   => LRBindings.successNel[Error]
        case "RLBindings"   => RLBindings.successNel[Error]
        case "Bindings"     => Bindings.successNel[Error]
        case "BinderLexeme" => c(0).asString.map(Lexeme(_))
        case x              => ParseError("Expected BinderSyntaxElement, not: " + x: String, buildAttribute(term)).failNel[BinderSyntaxElement]
      }
    })
  def parseBindingSyntaxElement(term: IStrategoTerm): Result[BindingSyntaxElement] = term.as[StrategoAppl].flatMap(
    { d =>
      val c = d.getAllSubterms()
      d.getConstructor().getName() match {
        case "BindingVar"    => BindingVar.successNel[Error]
        case "BindingBindee" => BindingBindee.successNel[Error]
        case "BindingLexeme" => c(0).asString.map(Lexeme(_))
        case x               => ParseError("Expected BindingSyntaxElement, not: " + x: String, buildAttribute(term)).failNel[BindingSyntaxElement]
      }
    })

  def parseBinding(term: IStrategoTerm): Result[BindingDef] =
    {
      val att = buildAttribute(term)
      term.$("BindingDef", subterms =>
        for (
          visibility <- parseVisibility(subterms(0): IStrategoTerm);
          name <- subterms(1).asString;
          tyA <- subterms(2).asType;
          tyB <- subterms(3).asType;
          forId <- subterms(4).asString;
          a <- subterms(5).as[StrategoAppl];
          e <- parseExpr(a);
          l <- subterms(6).mapOption(_.$("BindingSyntax", c => c(0).asStrategoList(parseBindingSyntaxElement)))
        ) yield BindingDef(visibility, name, tyA, tyB, forId, e, l, att))
    }

  def parseTyDecl(term: IStrategoTerm): Result[(String, Type)] =
    term.$("TyDecl", subterms =>
      for (
        id <- subterms(0) asString;
        tyapp <- subterms(1).as[StrategoAppl];
        ty <- parseType(tyapp)
      ) yield (id, ty))
  def parseTyDeclList(list: StrategoList): Result[List[(String, Type)]] =
    list.asScala.toList.map(parseTyDecl).sequence[({ type l[a] = Result[a] })#l, (String, Type)]

  def parseType(d: StrategoAppl): Result[Type] =
    {
      val name = d.getConstructor().getName()
      val c = d.getAllSubterms()
      val att = buildAttribute(d)

      name match {
        case "TyArrow"   => for (c0 <- c(0).asType; c1 <- c(1).asType) yield TyArrow(c0, c1, att)
        case "TyApp"     => for (c0 <- c(0).asType; c1 <- c(1).asType) yield TyApp(c0, c1, att)
        case "TyForall"  => for (c0 <- c(0).asString; c1 <- c(1).asKind; c2 <- c(2).asType) yield TyForall(c0, c1, c2, att)
        case "TyAbs"     => for (c0 <- c(0).asString; c1 <- c(1).asKind; c2 <- c(2).asType) yield TyAbs(c0, c1, c2, att)
        case "TyInt"     => TyInt(att).successNel[Error]
        case "TyString"  => TyString(att).successNel[Error]
        case "TyBool"    => TyBool(att).successNel[Error]
        case "TyVar"     => c(0) asString () map (TyVar(_, att))
        case "TyQVar"    => c(0) asString () map (TyQVar(_, att))
        case "TyRecord"  => for(
          c0 <- c(0).as[StrategoList] flatMap (parseTyDeclList(_));
          names = c0.map(_._1);
          duplicates =  names groupBy {x=>x} filter {case (_,lst) => lst.size > 1 } keys;
          _ <- if(duplicates.isEmpty) ().successNel[Error]
               else ContextError("Record type contains duplicate names: "+duplicates.mkString(", "), att).failNel[Unit]
      ) yield TyRecord(c0.toMap, att)
        case "TyVariant"  => for(
          c0 <- c(0).as[StrategoList] flatMap (parseTyDeclList(_));
          names = c0.map(_._1);
          duplicates =  names groupBy {x=>x} filter {case (_,lst) => lst.size > 1 } keys;
          _ <- if(duplicates.isEmpty) ().successNel[Error]
               else ContextError("Variant type contains duplicate names: "+duplicates.mkString(", "), att).failNel[Unit]
      ) yield TyVariant(c0.toMap, att)
        case "TyMu"      => for (c0 <- c(0).asType; c1 <- c(1).asType) yield TyMu(c0, c1, att)
        case "TyPair"    => for (c0 <- c(0).asType; c1 <- c(1).asType) yield TyPair(c0, c1, att)
        case "TyFst"     => for (c0 <- c(0).asType) yield TyFst(c0, att)
        case "TySnd"     => for (c0 <- c(0).asType) yield TySnd(c0, att)
        case "amb" =>
          for (c0 <- c(0).asStrategoList(_.asType))
            yield TyAmb(c0(0), c0(1), att)
      }
    }
  def parseKind(d: StrategoAppl): Result[Kind] =
    {
      val name = d.getConstructor().getName()
      val c = d.getAllSubterms()
      val att = buildAttribute(d)

      name match {
        case "KiArrow" => for (c0 <- c(0).asKind; c1 <- c(1).asKind) yield KiArrow(c0, c1, att)
        case "KiStar"  => KiStar(att).successNel[Error]
        case "KiPair"  => for (c0 <- c(0).asKind; c1 <- c(1).asKind) yield KiPair(c0, c1, att)
      }
    }

  def parseExpr(d: StrategoAppl): Result[Expr] =
    {
      val name = d.getConstructor().getName()
      val c = d.getAllSubterms()
      val att = buildAttribute(d)
      name match {
        case "Var"  => c(0) asString () map (Var(_, att))
        case "QVar" => c(0) asString () map (QVar(_, att))
        case "App"  => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr) yield App(c0, c1, att)
        case "amb" =>
          for (c0 <- c(0).asStrategoList(_.asExpr))
            yield ExprAmb(c0(0), c0(1), att)
        case "TApp"       => for (c0 <- c(0).asExpr; c1 <- c(1).asType) yield TApp(c0, c1, att)
        case "Abs"        => for (c0 <- c(0).asString; c1 <- c(1).asType; c2 <- c(2).asExpr) yield Abs(c0, c1, c2, att)
        case "TAbs"       => for (c0 <- c(0).asString; c1 <- c(1).asKind; c2 <- c(2).asExpr) yield TAbs(c0, c1, c2, att)
        case "Fold"       => for (c0 <- c(0).asType; c1 <- c(1).asExpr) yield Fold(c0, c1, att)
        case "Unfold"     => for (c0 <- c(0).asType; c1 <- c(1).asExpr) yield Unfold(c0, c1, att)
        case "Ascription" => for (c0 <- c(0).asExpr; c1 <- c(1).asType) yield Ascription(c0, c1, att)
        case "Int"        => for {
          c0 <- c(0) asString () 
          i <- try { c0.toInt.successNel[Error] } 
          catch { case e: NumberFormatException => ParseError(c0 + " is not a valid Int", att).failNel[Int]}
        }
         yield Num(i, att)
        
        case "String"     => c(0) asString () map (s => Str(s.dropRight(1).drop(1), att))
        case "True"       => Bool(true, att).successNel[Error]
        case "False"      => Bool(false, att).successNel[Error]
        case "If"         => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr; c2 <- c(2).asExpr) yield Cond(c0, c1, c2, att)
        case "Record"     => 
          for (c0 <- c(0).asStrategoList(parseField);
            names = c0.map(_._1);
            duplicates =  names groupBy {x=>x} filter {case (_,lst) => lst.size > 1 } keys;
            _ <- if(duplicates.isEmpty) ().successNel[Error]
                 else ContextError("Record contains duplicate names: "+duplicates.mkString(", "), att).failNel[Unit]
        ) yield Record(c0.toMap, att)
        case "Selection"  => for (c0 <- c(0).asExpr; c1 <- c(1).asString) yield Selection(c0, c1, att)
        case "Variant"    => for (c0 <- parseField(c(0)); c1 <- c(1).asType) yield Variant(c0._1, c0._2, c1, att)
        case "Case"       => for (c0 <- c(0).asExpr; c1 <- c(1).asStrategoList(parseBranch)) yield CaseOf(c0, c1, att)
        case "Add"        => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr) yield Add(c0, c1, att)
        case "Sub"        => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr) yield Sub(c0, c1, att)
        case "Mul"        => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr) yield Mul(c0, c1, att)
        case "Greater"    => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr) yield Greater(c0, c1, att)
        case "Div"        => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr) yield Div(c0, c1, att)
        case "CharOf"     => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr) yield CharOf(c0, c1, att)
        case "Concat"     => for (c0 <- c(0).asExpr; c1 <- c(1).asExpr) yield Concat(c0, c1, att)
        case "StrSize"    => c(0) asExpr () map (StrSize(_, att))
        case "LetSeq" =>
          val r = parseBindings(c(1)).flatMap { x =>
            x.map { c1 =>
              for (
                c0 <- c(0).asString;
                c2 <- c(2).asExpr
              ) yield LetSeq(c0, c1, c2, att): Expr
            }.sequence[({ type l[a] = Result[a] })#l, Expr]
          }
          val a = r.map(_.reduce[Expr]({ case (z, x) => ExprAmb(z, x) }))
          a

        case "LetPar" => 
          val r = parseBindings(c(1)).flatMap { x =>
            x.map { c1 =>
              for (
                c0 <- c(0).asString;
                c2 <- c(2).asExpr
              ) yield LetPar(c0, c1, c2, att): Expr
            }.sequence[({ type l[a] = Result[a] })#l, Expr]
          }
          val a = r.map(_.reduce[Expr]({ case (z, x) => ExprAmb(z, x) }))
          a
        case _ => ParseError("Unknown expr: " + d.getConstructor().getName() + " \nTerm: " + d, att).failNel[Expr]
      }
    }
  //handle amb, get all combinations of List of Bindings parser could mean 
  def parseBindings(term: IStrategoTerm): Result[List[List[Binding]]] = term match {
    case t: StrategoList => t.asStrategoList(parseBindings).map(xs =>
      TermRewriter.combinations(xs).map { x => x.flatMap { y => y.asInstanceOf[List[Binding]] } })
    case d: StrategoAppl =>
      val name = d.getConstructor().getName()
      val c = d.getAllSubterms()
      name match {
        case "amb" =>
          for (c0 <- c(0).asStrategoList(parseBindings))
            yield c0.flatten //println(c0.mkString("","\n",""))
        case _ => parseBindingTerm(term).map { x => List(List(x)) }
      }
  }

  def parseBranch(term: IStrategoTerm): Result[Branch] = term.$("Branch", c =>
    for (c0 <- c(0).asString; c1 <- c(1).asString; c2 <- c(2).asExpr) yield Branch(c0, c1, c2, buildAttribute(term)))

  def parseField(term: IStrategoTerm): Result[(String, Expr)] = term.$("Field", c =>
    for (c0 <- c(0).asString; c1 <- c(1).asExpr) yield (c0, c1))

  def parseBindingTerm(term: IStrategoTerm): Result[Binding] =
    term.as[StrategoAppl].flatMap(app =>
      app.getConstructor().getName() match {
        case "Binding" => term.$("Binding", c => for (
          c0 <- c(0).asString; c1 <- c(1).asString;
          c2 <- c(2).asExpr
        ) yield BindingN(c0, c1, c2, buildAttribute(term)))
        case "BindingWithoutVar" => term.$("BindingWithoutVar", c => for (
          c0 <- c(0).asString; 
          c1 <- c(1).asExpr
        ) yield BindingN(c0, "$dummy", c1, buildAttribute(term)))
        case "BindingAs" => term.$("BindingAs", c => for (
          c0 <- c(0).asString; c1 <- c(1).asString;
          c2 <- c(2).asType; c3 <- c(3).asExpr
        ) yield BindingAs(c0, c1, c2, c3, buildAttribute(term)))
        case x => ParseError("Expected Binding Term, not: " + x: String, buildAttribute(term)).failNel[Binding]
      })

}
