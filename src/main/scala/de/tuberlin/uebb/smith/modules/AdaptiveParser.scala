package de.tuberlin.uebb.smith.modules

import java.io.{ File, FileInputStream, FileOutputStream, FileWriter }
import org.spoofax.jsglr.io.FileTools
import java.io.BufferedReader
import java.io.FileReader
import org.spoofax.interpreter.terms.IStrategoTerm
import org.kiama.output.PrettyPrinter
import scalaz._
import Scalaz._
import org.spoofax.terms.StrategoAppl
import org.spoofax.terms.StrategoList
import org.spoofax.terms.StrategoString
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import scala.collection.mutable.{ Map => MutableMap }
import TypeSynonyms._
import Syntax._
import LookUp._
import scala.sys.process._
import scala.util._
import scala.collection.mutable.HashMap
trait AdaptiveParser extends Parser {
  self: SDFGenerator =>

  override def parseSDF(input: String, start: String, offset: Position, fileName: String): Result[IStrategoTerm] = {
    if (dirty)
      createCurrentTable()
    super.parseSDF(input, start, offset, fileName)
  }

  override def parseBindingTerm(is: IStrategoTerm): Result[Binding] =
    is.as[StrategoAppl].flatMap({ app =>
      val name = app.getConstructor().getName()
      if (syntaxBindingTable.contains(name)) {
        //if syntax is overloaded its not clear which
        //binding it is, so we produces every version
        val possibleMeanings: Result[List[Binding]] =
          syntaxBindingTable(name).map { x =>
            val id = x._1
            val bpostion = x._2
            val c = app.getAllSubterms()
            //Could be VAR BINDEE or BINDEE VAR or BINDEE
            if (bpostion == BindeeLeft)
              (c(1).asString |@| c(0).asExpr) { BindingN(id, _, _, buildAttribute(is)) }
            else if (bpostion == BindeeRight)
              (c(0).asString |@| c(1).asExpr) { BindingN(id, _, _, buildAttribute(is)) }
            else
              (c(0).asExpr).map { BindingN(id, "$dummy", _, buildAttribute(is)) }
          }.sequence[({ type l[a] = Result[a] })#l, Binding]
        possibleMeanings.map(x => x.reduce[Binding]({ case (a, b) => BindingAmb(a, b) }))
      } else
        super.parseBindingTerm(is)
    })

  override def parseExpr(d: StrategoAppl): Result[Expr] =
    {
      val name = d.getConstructor().getName()
      if (syntaxValTable.contains(name)) {
        //if syntax is overloaded its not clear which
        //function has to be called, so we produces every version
        val possibleMeanings: Result[List[Expr]] =
          syntaxValTable(name).map { x =>
            val id = x._1
            val ty = x._2
            val c = d.getAllSubterms()
            val r = parseAppSyntax(c.toList, ty, Var(id))
            r
          }.sequence[({ type l[a] = Result[a] })#l, Expr]
        possibleMeanings.map(x => x.reduce[Expr]({ case (a, b) => ExprAmb(a, b) }))
      } else if (syntaxBinderTable.contains(name)) {
        val possibleMeanings: Result[List[Expr]] =
          syntaxBinderTable(name).map { x =>
            val id = x._1
            val binding = x._2
            val binderType = x._3
            val c = d.getAllSubterms()
            val syn = x._2.filter(g => !g.isInstanceOf[Lexeme])
            val bodypos = syn.indexOf(BinderBody)
            val body = c.toList(bodypos)
            val bindingspos = if (bodypos == 0) 1 else 0
            val bindings = c.toList(bindingspos)
            val r: Result[List[Expr]] =
              for {
                bo <- body.asExpr
                x <- parseBindings(bindings)  // x : List[List[Binding]]
              } yield {
                x.map { (bs :List[Binding]) =>
                  (syn(bindingspos), binderType) match {
                    case (Bindings,   Seq) => LetSeq(id, bs, bo, buildAttribute(d))
                    case (RLBindings, Seq) => LetSeq(id, bs, bo, buildAttribute(d))
                    case (LRBindings, Seq) => LetSeq(id, bs.reverse, bo, buildAttribute(d))
                    case (Bindings,   Par) => LetPar(id, bs, bo, buildAttribute(d))
                    case (RLBindings, Par) => LetPar(id, bs, bo, buildAttribute(d))
                    case (LRBindings, Par) => LetPar(id, bs.reverse, bo, buildAttribute(d))
                  }
                }
              }
            //if we get multiple versions of this LET
            //we collected them in ExprAmb and let the later stages handle it
            val a = r.map(_.reduce[Expr]({ case (z, x) => ExprAmb(z, x) }))
            a
          }.sequence[({ type l[a] = Result[a] })#l, Expr]
        possibleMeanings.map(x => x.reduce[Expr]({ case (a, b) => ExprAmb(a, b) }))
      } else
        super.parseExpr(d)
    }

  override def parseType(d: StrategoAppl): Result[Type] =
    {
      val name = d.getConstructor().getName()
      if (syntaxTypeTable.contains(name)) {
        //if syntax is overloaded its not clear which
        //function has to be called, so we produces every version
        val possibleMeanings: Result[List[Type]] =
          syntaxTypeTable(name).map { x =>
            val id = x
            val c = d.getAllSubterms()
            val r = parseAsArray(c, (_.asType)).map { x =>
              x.foldLeft(TyVar(id): Type)({ case (a, t) => TyApp(a, t, a.att merge t.att) })
            }
            r
          }.sequence[({ type l[a] = Result[a] })#l, Type]
        possibleMeanings.map(x => x.reduce[Type]({ case (a, b) => TyAmb(a, b) }))
      } else
        super.parseType(d)
    }

  def parseAppSyntax(d: List[IStrategoTerm], t: Type, z: Expr): Result[Expr] = (d, t) match {
    case (e :: Nil, TyArrow(l, r, _)) => e.asExpr.map { x => App(z, x, z.att merge x.att)}
    case (e :: ds, TyArrow(l, r, _)) => for {
      ee <- e.asExpr
      er <- parseAppSyntax(ds, r, App(z, ee, z.att merge ee.att))
    } yield er
    case (e :: Nil, TyForall(_, _, r, _)) => e.asType.map { x => TApp(z, x, z.att merge x.att) }
    case (e :: ds, TyForall(_, _, r, _)) => for {
      ee <- e.asType
      er <- parseAppSyntax(ds, r, TApp(z, ee, z.att merge ee.att))
    } yield er
    case _ => InternalError("Only here to handle warnings", EmptyAttribute).failNel[Expr]
  }
}
