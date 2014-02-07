package de.tuberlin.uebb.smith.modules

import org.spoofax.jsglr.io.FileTools
import java.io.BufferedReader
import java.io.FileReader
import java.io.File
import org.spoofax.interpreter.terms.IStrategoTerm
import org.kiama.output.PrettyPrinter
import scalaz._
import Scalaz._
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.collection.mutable.{ Map => MutableMap }
import scala.collection.immutable.ListMap
import TypeSynonyms._
import Syntax._
import LookUp._
import TermRewriter._

trait Driver {
  self: AdaptiveParser with DependenceAnalysis with ContextChecker with TypeChecker with SDFGenerator =>

  val moduleHeaderCache: MutableMap[Identifier, ModuleHeader] = MutableMap()
  def run(startModule: String, cmdExpr : String) =
    {
      val modules = compile(startModule, cmdExpr)
      cleanUp()
      val res = modules.flatMap(Interpreter.run(_))
      res.fold(l => 
         println(l.list.foldLeft("Errors:"){ case (z, e) => z + "\n\n" + e }),
        {x => println("Result:\n"+ x)})
    }

  def compile(startModule: String, cmdExpr : String): Result[Context] = {
    //adding dummy module for command line expression
    //which imports the given module
    moduleHeaderCache += (ID("cmd") -> ModuleHeader(ID("cmd"),List(
      Import(Private, NotQualified(startModule), List())),
      "VAL cmdLineArg = " + cmdExpr)(Position(0,0), "command line argument"))

    val initContext = Context(SymbolTable(Map(), Map()), Map(), Map(), ListMap())
    val modules = getTopologicalOrder(startModule).flatMap({ res =>
      //modules in topological order
      val mhs = res.map(moduleHeaderCache(_))

      mhs.foldLeft(initContext.successNel[Error]: Result[Context])({ (ctxV, mh) =>
        ctxV.flatMap(ctx =>
          for (
            dic <- buildRenaming(mh, ctx.modules);
            _ = initParser(); //Resets position offsets
            _ = clearSDF(); //Sets back SDF definition back to core language
            _ <- buildImportedSyntax(dic, ctx.modules, ctx.st, FileAttribute(mh.fileName));
            (ds, newDic, newCtx) <- step(mh.rest, dic, mh.name, ctx, mh.offset, mh.fileName);
            (tds, vds, bds, bids, brds) = splitDefs(ds)
          ) yield newCtx.copy(
            modules = ctx.modules + ((mh.name,
              Module(mh.name, mh.imports, tds.map(x => (x.name, x)).toMap,
                ListMap() ++ vds.map(x => (x.name, x)), bds.map(x => (x.name, x)).toMap,
                bids.map(x => (x.name, x)).toMap, brds.map(x => (x.name, x)).toMap, newDic)))))
      })
    })
    return modules
  }

  /**
    * Handles a "rest" of defs step by step.
    * Returns the context containing all new definitions.
    * Every syntax extension is added to the SDF definition.
    */
  def step(input: String,
           dic: Dictionary,
           currentModule: Identifier,
           ctx: Context,
           offset: Position,
           fileName: String): Result[(List[Def], Dictionary, Context)] = {
    input match {
      case "" => (Nil, dic, ctx).successNel[Error]
      case s => for (
        (d1, rest, offset2) <- parseSDF(input, "DefRest", offset, fileName).flatMap(parseDefRest(_, offset, fileName));
        (d2, ctx2) <- handleAmbiguousDef(d1, dic, currentModule, ctx);
        (dic2) <- dic.addDef(d2, currentModule); //Update dictionary
        r <- handleSyntaxExtention(d2, dic2, currentModule, ctx2);
        (ds, dic3, ctxR) <- step(rest, dic2, currentModule, ctx2, offset2, fileName)
      ) yield (d2 :: ds, dic3, ctxR)
    }
  }

  /**
    * Creates every possible variation of the def and runs handleDef on all of them.
    * If only one returns without and error, that is the right one.
    */
  def handleAmbiguousDef(d: Def, dic: Dictionary, currentModule: Identifier, ctx: Context): Result[(Def, Context)] =
  {
    //disambiguate and type check all variants
    val results =
        disambiguate(d).map { disDef => handleDef(disDef, dic, currentModule, ctx)}
            // split failures and successes
            val (fails, suc) = results.partition(_.isFailure)
            //a little type magic to give all errors back
            if (suc.size == 0) { //no successes
              val msgs = fails.sequence[({ type l[a] = Result[a] })#l, (Def, Context)]
              if (fails.size == 1) // just one fail, so normal typ error
                return msgs.map { x =>
                  // should never happen, because fails should be a List of Failures
                  assert(false)
                  (d, ctx)
                }
              else //more than one fail, expression was ambiguous 
                return AmbError("Definition is ambiguous, but could not determine correct one. Possible errors:\n" +
                msgs.fold(l => l.list.mkString("", "\n***** Another version *****\n", ""), r => ""), d.att).failNel[(Def, Context)]
            } else if (suc.size != 1) { //more than one success, unclear which is the right one
              val msgs = suc.sequence[({ type l[a] = Result[a] })#l, (Def, Context)].fold(l => "",
                r => r.map { case (d, ctx) => d }.mkString("", "\n", ""))
              return AmbError("Definition "+ d.name+" is ambiguous, possible meanings:\n " + msgs, d.att).failNel[(Def, Context)]
            } else //everything went better than expected
              suc(0)
  }

  /**
    * Adds the syntax definitions to sdf.
    * Should only be used, after the def is disambiguated and typchecked.
    */ 
  def handleSyntaxExtention(d: Def, dic: Dictionary, currentModule: Identifier, ctx: Context) : Result[Unit] = d match {
    case vd: ValDef =>
      lookUpType(vd.name, ctx.st, dic, currentModule, d.att).flatMap{t =>
        vd.syn.map(buildProductionForVal(_, vd.name, t, d.att)).getOrElse(().successNel[Error])}
    case td: TypeDef =>
      lookUpKind(td.name, ctx.st, dic, currentModule, d.att).flatMap{ k =>
        td.syn.map(buildProductionForType(_, td.name, k, d.att)).getOrElse(().successNel[Error]) }
    case bd: BinderDef =>
      bd.syn.map(buildProductionForBinder(bd.binderType,_, bd.name, bd.att)).getOrElse(().successNel[Error])
    case bd: BindingDef =>
      bd.syn.map(buildProductionForBinding(_, bd.name, bd.att)).getOrElse(().successNel[Error])
    case bd: BracketDef =>
      buildPriority(bd, bd.att)
  }


  /**
    * Typechecks the definition and adds the definition into the context.
    */
  def handleDef(d: Def, dic: Dictionary, currentModule: Identifier, ctx: Context): Result[(Def, Context)] =
    {
      val res = d match {
        case vd: ValDef =>
          {
            val rExpr = removeNamesFromExpr(vd.e)
            for {
              t <- computeType(rExpr, ctx, dic, currentModule)
              nst <- (ctx.st + (currentModule ~ vd.name, t, vd.att))
            } yield (vd.copy(e = rExpr), ctx.copy(st = nst))
          }
        case td: TypeDef =>
          {
            val rType = removeNamesFromType(td.t)
              for {
                k <- kindOf(rType, ctx.st, dic, currentModule, Nil)
                et <- evalTypeV(rType, ctx.st, dic, currentModule)
                nst1 <- (ctx.st + (currentModule ~ td.name, k, td.att))
                nst2 <- (nst1 + (currentModule ~ td.name, et, td.att))
              } yield (td.copy(t = et), ctx.copy(st = nst2))
          }
        case bd: BinderDef =>
          {
            val uType = removeNamesFromType(bd.u)
            val wType = removeNamesFromType(bd.w)
            val tExpr = removeNamesFromExpr(bd.e)
            val nbd = bd.copy(e = tExpr)
            for {
              t <- checkTypeOfBinder(uType, wType, tExpr, ctx, dic, currentModule)
            } yield (nbd, ctx.copy(binder = ctx.binder + (currentModule ~ nbd.name -> nbd)))
          }
        case bd: BindingDef =>
          {
            lookUpBinder(bd.forId, currentModule, ctx, dic, bd.att).flatMap { binder =>
              val uType = removeNamesFromType(binder.u)
              val wType = removeNamesFromType(binder.w)
              val vType = removeNamesFromType(bd.v)
              val rType = removeNamesFromType(bd.r)
              val tExpr = removeNamesFromExpr(bd.e)
              val nbd = bd.copy(e = tExpr)
              for {
                t <- if(binder.binderType == Seq) 
                     checkTypeOfSeqBinding(bd.name,uType, wType, vType, rType, tExpr, ctx, dic, currentModule)
                     else 
                     checkTypeOfParBinding(bd.name,uType, wType, vType, rType, tExpr, ctx, dic, currentModule)
              } yield (nbd, ctx.copy( bindings = ctx.bindings + (currentModule ~ nbd.name -> nbd)))
            }
          }
        case bd: BracketDef =>
            (bd, ctx).successNel[Error]

      }
      res
    }

  def splitDefs(ds: List[Def]): (List[TypeDef], List[ValDef], List[BinderDef], List[BindingDef], List[BracketDef]) =
    (ds.filter(_.isInstanceOf[TypeDef]).map(_.asInstanceOf[TypeDef]),
      ds.filter(_.isInstanceOf[ValDef]).map(_.asInstanceOf[ValDef]),
      ds.filter(_.isInstanceOf[BinderDef]).map(_.asInstanceOf[BinderDef]),
      ds.filter(_.isInstanceOf[BindingDef]).map(_.asInstanceOf[BindingDef]),
      ds.filter(_.isInstanceOf[BracketDef]).map(_.asInstanceOf[BracketDef]))

  var workingDir: String = ""
  def qualifiedNameToPath(qn: String) = qn.replace('.', '/')
  def buildPath(name: String) = workingDir + "/" + qualifiedNameToPath(name) + ".sm"

  /**
    * Load a moduleheader.
    * If it is in cache, that version is returned
    * otherwise its loaded from disk.
    */
  def loadModuleHeader(moduleName: Identifier): Result[ModuleHeader] =
    {
      val inputFile = buildPath(moduleName.toPath())
      if(! (new File(inputFile).exists()))
        return ModuleError("Could not find "+ moduleName + " under " +inputFile, EmptyAttribute).failNel[ModuleHeader]
      val input = FileTools.loadFileAsString(new BufferedReader(new FileReader(inputFile)));
      val m = for {
        t <- parseSDF(input, "ModuleRest", Position(0,0), inputFile)
        x <- parseModuleHeader(t.asInstanceOf[IStrategoTerm], inputFile)
        _ <- if(moduleName != x.name) 
               ModuleError("Trying to load module " + moduleName + 
                            " but found module with name " + x.name, EmptyAttribute).failNel[ModuleHeader]
        else ().successNel[Error]
      } yield {
        moduleHeaderCache += x.name -> x
        x
      }
      m
    }

}
