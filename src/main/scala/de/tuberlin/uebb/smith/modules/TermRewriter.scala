package de.tuberlin.uebb.smith.modules

import org.kiama.output.PrettyPrinter
import org.kiama.rewriting.Rewriter._
import org.kiama.rewriting._
import scalaz._
import Scalaz._
import TypeSynonyms._
import Syntax._
import scala.collection.mutable.ListBuffer

case object TermRewriter extends TermRewrite
trait TermRewrite extends Rewriter {



  def disambiguate[A](x: A) : List[A] = rewrite(disambiguateRule)(x).asInstanceOf[List[A]]
  def disambiguateRule: Strategy = rule {
    case DefAmb(a: Def, b: Def) =>
      rewrite(disambiguateRule)(a).asInstanceOf[List[_]] ++
      rewrite(disambiguateRule)(b).asInstanceOf[List[_]]
    case ExprAmb(a: Expr, b: Expr, att) =>
      rewrite(disambiguateRule)(a).asInstanceOf[List[_]] ++
      rewrite(disambiguateRule)(b).asInstanceOf[List[_]]
    case TyAmb(a: Type, b: Type, att) =>
      rewrite(disambiguateRule)(a).asInstanceOf[List[_]] ++
      rewrite(disambiguateRule)(b).asInstanceOf[List[_]]
    case BindingAmb(a, b) =>
      rewrite(disambiguateRule)(a).asInstanceOf[List[_]] ++
      rewrite(disambiguateRule)(b).asInstanceOf[List[_]]
    case x =>
      val ch = getChildren(x)
      if(ch.isEmpty) {
        List(x)
      }
      else {
        val chdis = ch.map({ rewrite(disambiguateRule)(_) }) // get all disambiguate children
                                                             //create all combinations of the disambiguated children
        val p = combinations(chdis.asInstanceOf[List[List[AnyRef]]])
        //use dup from Kiama to recreate the term with every combination
        val xs = 
          if (x.isInstanceOf[Product])
            for { newchildren <- p } yield dup(x.asInstanceOf[Product], newchildren.toArray)
          else if (x.isInstanceOf[Map[_,_]])
            for { newchildren <- p } yield {
              newchildren.map(_.asInstanceOf[(AnyRef,AnyRef)]).toMap
            }
        xs
      }
  }
  
  def combinations(ll: List[List[AnyRef]]): List[List[AnyRef]] = ll match {
    case Nil      => Nil
    case x :: Nil => x.map { List(_) }
    case x :: xs  => combinations(xs).flatMap({ ys => x.map({ xx => xx :: ys }) })
  }

  /**
   * Number of children of the given value.
   */
  protected def arity(x: Any): Int = {
    var c = 0;
    all(queryf {
      case a => c += 1;
    })(x)
    c
  }

  /**
   * Get the children of the given value.
   */
  protected def getChildren(x: Any): List[Any] = {
    val l = new ListBuffer[Any]()
    all(queryf {
      case a => l += a
    })(x)
    l.toList
  }

  /**
   * Substitute type index pointing to outer abstraction (TyAbs,TAbs or TyForall)
   */
  def substituteTopTypeAndShift[A](t: Type, in: A): A = {
    substituteTypeIndexRule(0, t)(in).get.asInstanceOf[A]
  }

  def substituteTypeIndexRule(i: Int, sub: Type): Strategy = rule {
    case TyForall(x, k, b, att)           => TyForall(x, k, substituteTypeIndexRule(i + 1, shift(sub))(b).get.asInstanceOf[Type], att)
    case TyAbs(x, k, b, att)              => TyAbs(x, k, substituteTypeIndexRule(i + 1, shift(sub))(b).get.asInstanceOf[Type], att)
    case TyIndex(it, v, att) if (i == it) => sub 
    case TyIndex(it, v, att) if (i < it)  => TyIndex(it - 1, v, att)
    case x                                => all(substituteTypeIndexRule(i, sub))(x).get
  }

  def shift[T](t: T) =
    shiftTypeRule(0)(t).get.asInstanceOf[T]

  def shiftTypeRule(i: Int): Strategy = rule {
    case TyIndex(it, v, att) if (it >= i) => TyIndex(it + 1, v, att)
    case TyForall(x, k, b, att)           => TyForall(x, k, shiftTypeRule(i + 1)(b).get.asInstanceOf[Type], att)
    case TyAbs(x, k, b, att)              => TyAbs(x, k, shiftTypeRule(i + 1)(b).get.asInstanceOf[Type], att)
    case x                           => all(shiftTypeRule(i))(x).get
  }

  def leftShift[T](t: T) = leftShiftTypeRule(0)(t).get.asInstanceOf[T]
  def leftShiftTypeRule(i: Int): Strategy = rule {
    case TyIndex(it, v, att) if (it >= i) => TyIndex(it - 1, v, att)
    case TyAbs(x, k, b, att)              => TyAbs(x, k, leftShiftTypeRule(i + 1)(b).get.asInstanceOf[Type], att)
    case x                           => all(leftShiftTypeRule(i))(x).get
  }


  /**
   * Removes local bound type variables and replaces them with an index (statix distance)
   */
  def removeNamesFromType(e: Type): Type = recRuleRemoveNamesFromType()(e).get.asInstanceOf[Type]

  def recRuleRemoveNamesFromType(): Strategy =
    rule {
      case TyForall(x, t, e, att) => TyForall(new ShowId(x), t, ruleRemoveNamesFromType(0, x)(removeNamesFromType(e)).get.asInstanceOf[Type], att)
      case TyAbs(x, k, e, att)    => TyAbs(new ShowId(x), k, ruleRemoveNamesFromType(0, x)(removeNamesFromType(e)).get.asInstanceOf[Type], att)
      case x                 => all(recRuleRemoveNamesFromType)(x).get
    }

  def ruleRemoveNamesFromType(i: Int, id: Identifier): Strategy = rule {
    case TyAbs(x, k, t, att)        => TyAbs(x, k, ruleRemoveNamesFromType(i + 1, id)(t).get.asInstanceOf[Type], att)
    case TyForall(x, k, t, att)     => TyForall(x, k, ruleRemoveNamesFromType(i + 1, id)(t).get.asInstanceOf[Type], att)
    case TyVar(n, att) if (n == id) => TyIndex(i, new ShowId(id), att)
    case x                     => all(ruleRemoveNamesFromType(i, id))(x).get
  }

  /**
   * Removes local bound type variables and replaces them with an index (static distance)
   */
  def removeNamesFromExpr(e: Expr): Expr =  recRuleRemoveNamesFromExpr()(e).get.asInstanceOf[Expr] 
  /**
    * Recursive get a term, get the subterms where all bound variables are replaced by an index.
    * For every kind of abstraction replace the bounded var with an index in the subterms.
    */
  def recRuleRemoveNamesFromExpr(): Strategy =
    rule({
      case Abs(x, t, e, att) => Abs(x, removeNamesFromType(t), removeNamesFromExpr(e), att)
      case TAbs(x, k, e, att)    => TAbs(x, k, (ruleRemoveTypeNamesFromTerm(0, x)(removeNamesFromExpr(e))).get.asInstanceOf[Expr], att)
      case TApp(f, t, att)       => TApp(removeNamesFromExpr(f), removeNamesFromType(t), att)
      case Ascription(f, t, att) => Ascription(removeNamesFromExpr(f), removeNamesFromType(t), att)
      case x => (all(recRuleRemoveNamesFromExpr)(x)).get
    })


  def ruleRemoveTypeNamesFromTerm(i: Int, id: Identifier): Strategy = rule {
    case TAbs(x, k, e, att)         => TAbs(x, k, ruleRemoveTypeNamesFromTerm(i + 1, id)(e).get.asInstanceOf[Expr], att)
    case TyVar(n, att) if (n == id) => TyIndex(i, new ShowId(id), att)
    case x                     => all(ruleRemoveTypeNamesFromTerm(i, id))(x).get
  }
}
