package de.tuberlin.uebb.smith.modules

import TypeSynonyms._
import scalaz._
import Scalaz._
import Syntax._
import LookUp._
import TermRewriter._
import scala.collection.mutable.{ Map => MutableMap }

object Interpreter extends Desugarer with TypeChecker{

  def run(ctx: Context): Result[Expr] = {
    /**
      * Collect all interpreted values in this map under the full qualified name.
      */
    val values: MutableMap[Identifier, Expr] = MutableMap()
    /**
      * The last interpreted expression.
      * Usually the command line argument.
      */
    var last: Expr = Num(-1)
    //modules are in topological order
    ctx.modules.map {
      case (currentModule, module) =>
        //VAL defs are in order of appearance
        module.valDef.map {
          case (valId, valDef) =>

            val ee = for {
              de <- desugarExpr(valDef.e, currentModule, ctx, module.dic)
              ev <- eval(de, module.dic, values, Map())
            } yield ev
            ee match {
              case Success(e) =>
                last = e
                values += (currentModule ~ valId -> e)
              case err => return err
            }

        }
    }
    return last.successNel[Error]
  }


  def eval(e: Expr, dic: Dictionary, map : MutableMap[Identifier, Expr],  local : Map[Identifier, Expr]): Result[Expr] =
    {
      val res = e match {
        case Var(x, att) => if(local.contains(x)) local(x).successNel[Error]
        else
          (dic(x, att)  ) match {
            case Success(rid) => map(rid).successNel[Error]
            case _ => InternalError("Could not find var " + x + " - this should not happen." 
                + dic,att).failNel[Expr]
          }
        case QVar(x, att) =>
          (dic(x, att)  ) match {
            case Success(rid) => map(rid).successNel[Error]
            case _ => InternalError("Could not find qvar " + x + " - this should not happen." 
                ,att).failNel[Expr]
          }
        case Record(l: Map[String, Expr], att) =>
          (l.map { case (k, v) => eval(v, dic, map, local).map(v => (k, v)) }).toList.
            sequence[({ type l[a] = Result[a] })#l, (String, Expr)].map(x => Record(x.toMap))
        case Selection(r, s, att)  => eval(r,  dic, map, local).map(x => x.asInstanceOf[Record].l(s))
        case Ascription(e, _, att) => eval(e, dic, map, local)
        case Variant(s, e, t, att) => eval(e,  dic, map, local).map(x => Variant(s, x, t, att))
        case TApp(f, x, att)       => {
          for {
            fe <- eval(f,  dic, map, local)
            body = fe.asInstanceOf[TAbs].e
            ee = substituteTopTypeAndShift(x, body)
            re <- eval(ee, dic, map, local)
          } yield re
        }
        case Fold(_, e, att)       => eval(e, dic, map, local)
        case Unfold(_, e, att)     => eval(e, dic, map, local)
        case CaseOf(e, cases, att) => eval(e, dic, map, local).flatMap { x =>
          val xx = x.asInstanceOf[Variant]
          val br = cases.filter { b => b.a == xx.s }
          if (br.isEmpty)
            InterpreterError("No branch matches " + x + " with tag "+ xx.s, att).failNel[Expr]
          else
            eval(br(0).e, dic, map, local + (ID(br(0).b) -> xx.e)) 
        }
        case App(l, x, att) =>
          {
            for (
              abs <- eval(l, dic, map, local);
              xx <- eval(x, dic, map, local);
              y <- apply(abs, xx, map)
            ) yield y
          }
        case Add(x, y, att) =>
          {
            for {
              x2 <- eval(x, dic, map, local)
              y2 <- eval(y, dic, map, local)
              res = x2.asInstanceOf[Num].n.toLong + y2.asInstanceOf[Num].n.toLong
              _ <- if (res < Int.MinValue || res > Int.MaxValue) 
                      InterpreterError("Int out of bounds", att).failNel[Unit]
              else ().successNel[Error]
            } yield Num(res.toInt)
          }
        case Sub(x, y, att) =>
          {
            for {
              x2 <- eval(x, dic, map, local)
              y2 <- eval(y, dic, map, local)
              res = x2.asInstanceOf[Num].n.toLong - y2.asInstanceOf[Num].n.toLong
              _ <- if (res < Int.MinValue || res > Int.MaxValue) 
                       InterpreterError("Int out of bounds", att).failNel[Unit]
              else ().successNel[Error]
            } yield Num(res.toInt)
          }
        case Mul(x, y, att) =>
          {
            for {
              x2 <- eval(x, dic, map, local)
              y2 <- eval(y, dic, map, local)
              res = x2.asInstanceOf[Num].n.toLong * y2.asInstanceOf[Num].n.toLong
              _ <- if (res < Int.MinValue || res > Int.MaxValue) 
                       InterpreterError("Int out of bounds", att).failNel[Unit]
              else ().successNel[Error]
            } yield Num(res.toInt)
          }
        case Div(x, y, att) =>
          {
            for {
              x2 <- eval(x, dic, map, local)
              y2 <- eval(y, dic, map, local)
              _ <- if (y2.asInstanceOf[Num].n == 0)
                       InterpreterError("division by zero", att).failNel[Unit]
                    else ().successNel[Error]
              res = x2.asInstanceOf[Num].n.toLong / y2.asInstanceOf[Num].n.toLong
              _ <- if (res < Int.MinValue || res > Int.MaxValue)
                       InterpreterError("Int out of bounds", att).failNel[Unit]
                    else ().successNel[Error]
            } yield Num(res.toInt)
          }
        case Greater(x, y, att) =>
          {
            for (
              xx <- eval(x, dic, map, local);
              yy <- eval(y, dic, map, local)
            ) yield Bool(xx.asInstanceOf[Num].n > yy.asInstanceOf[Num].n)
          }
        case Concat(x, y, att) =>
          {
            for (
              xx <- eval(x, dic, map, local);
              yy <- eval(y, dic, map, local)
            ) yield Str(xx.asInstanceOf[Str].s + yy.asInstanceOf[Str].s)
          }
        case CharOf(x, y, att) =>
          {
            for (
              xx <- eval(x, dic, map, local);
              yy <- eval(y, dic, map, local);
              _ <- if (xx.asInstanceOf[Str].s.size > yy.asInstanceOf[Num].n)
                ().successNel[Error]
              else
                InterpreterError("String index out of bound " + xx.asInstanceOf[Str].s.size + " > " + yy.asInstanceOf[Num].n, att).failNel[Unit]
            ) yield Str(xx.asInstanceOf[Str].s(yy.asInstanceOf[Num].n).toString)
          }
        case StrSize(x, att) =>
          {
            for (
              xx <- eval(x, dic, map, local)
            ) yield Num(xx.asInstanceOf[Str].s.size)
          }
        case Cond(con, t, e, att) =>
          {
            eval(con, dic, map, local).flatMap(ccon =>
              if (ccon.asInstanceOf[Bool].b)
                eval(t, dic, map, local)
              else
                eval(e, dic, map, local))
          }
        case x@Num(n, att)       => x.successNel[Error]
        case x@Str(s, att)       => x.successNel[Error]
        case x@Bool(b, att)      => x.successNel[Error]
        case abs@Abs(a, t, b, att) => 
          Closure(a, t, b, dic, local, att).successNel[Error]
        case tabs@TAbs(x, k, b, att)    => eval(b, dic, map, local).map( TAbs(x,k,_, att)) 
        case cl : Closure => cl.successNel[Error]
        case x => InternalError("Found " + x.getClass+"\n" +x +" in Interpreter", x.att).failNel[Expr]
      }
      res 
    }

  def apply(c: Expr, arg: Expr, map: MutableMap[Identifier, Expr]) : Result[Expr] = c match {
    case Closure(a,t, b, dic, local, att) => eval(b, dic, map, local+(a -> arg))
    case _ => InternalError("Aplly on non closure", c.att).failNel[Expr]
  }
}
