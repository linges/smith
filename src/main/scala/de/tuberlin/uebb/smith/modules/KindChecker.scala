package de.tuberlin.uebb.smith.modules
import org.kiama.output.PrettyPrinter
import scalaz._
import Scalaz._
import TypeSynonyms._
import Syntax._
import Unification._
import LookUp._
import TermRewriter._

trait KindChecker {
  self: TypeChecker =>
  def evalTypeV(t: Type, st: SymbolTable, dic: Dictionary, cm: Identifier): Result[Type] =
    {
      try {
        val et = evalRule(st, dic, cm)(t).get.asInstanceOf[Type] 
        et.successNel[Error]
      } catch { 
        case FailWrapper(Fail(t, e)) => NonEmptyList(e.head, e.tail.toSeq: _*).fail[Type]
      }
    }

  def evalType(t: Type, st: SymbolTable, dic: Dictionary, cm: Identifier): TypeResult =
    {
      try {
        val et = evalRule(st, dic, cm)(t).get.asInstanceOf[Type] 
        lift(et)
      } catch {
        case FailWrapper(f) => f
      }
    }

  def evalRule(st: SymbolTable, dic: Dictionary, cm: Identifier): Strategy = rule {
    case TyVar(id, att)      => (lookup(id, st, dic, cm, att) & { x => evalType(x, st, dic, cm) }).$$
    case TyQVar(id, att)     => (lookup(id, st, dic, cm, att) & { x => evalType(x, st, dic, cm) }).$$
    case TyAbs(i, k, t, att) => (evalType(t, st, dic, cm) & { x => lift(TyAbs(i, k, x, att)) }).$$
    case TyApp(fun, arg, att) => (evalType(fun, st, dic, cm) & { ff =>
      evalType(arg, st, dic, cm) & { xx =>
        ff match {
          case TyAbs(i, k, body, att) => {
            val subs = substituteTopTypeAndShift[Type](xx, body)
            val res = evalType(subs, st, dic, cm)
            res
          }
          case UnknownType => lift(UnknownType)
          //we use this in places where not every local bound variable is 
          //known, so sometimes we can't eval further
          case _           => lift(TyApp(ff, xx, att))
        }
      }
    }).$$
    case TyPair(a, b, att) => 
      (evalType(a, st, dic, cm) & { aa =>
      evalType(b, st, dic, cm) & { bb =>
        lift(TyPair(aa,bb,att)) }}).$$
    case TyFst(t, att) => (evalType(t, st, dic, cm) & { tt =>
      tt match {
        case TyPair(a,b,att) => lift(a)
        case x => error(att, "Exptected type pair not : " +x )
      }}).$$
    case TySnd(t, att) => (evalType(t, st, dic, cm) & { tt =>
      tt match {
        case TyPair(a,b,att) => lift(b)
        case x => error(att, "Exptected type pair not : " +x )
      }}).$$

    case x => all(evalRule(st, dic, cm))(x).get
  }

  def checkType(t: Type, st: SymbolTable, dic: Dictionary, cm: Identifier, idx: List[Kind]): TypeResult =
    {
      kindOf(t, st, dic, cm, idx).fold(
        e => Fail(UnknownType, e.list),
        r => { lift(t) })
    }

  def kindOf(t: Type, st: SymbolTable, dic: Dictionary, cm: Identifier, idx: List[Kind]): Result[Kind] = {
    t match {
      case TyArrow(l, r, att) => 
        for { ll <- kindOf(l, st, dic, cm, idx)
              rr <- kindOf(r, st, dic, cm, idx)
        } yield KiStar(att)
      case UnknownType => KiStar().successNel[Error]
      case TyInt(att) => KiStar(att).successNel[Error]
      case TyBool(att) => KiStar(att).successNel[Error]
      case TyString(att) => KiStar(att).successNel[Error]
      case TyForall(id, k, t, att) =>
            kindOf(t, st, dic, cm, k :: idx).map {  _ => KiStar(att) }
      case TyRecord(l, att) =>
        (l.values.map(kindOf(_, st, dic, cm, idx))
        ).toList.sequence[({ type l[a] = Result[a] })#l, Kind].map{ _ => KiStar(att)}
      case TyVariant(l, att) =>
        (l.values.map(kindOf(_, st, dic, cm, idx))
        ).toList.sequence[({ type l[a] = Result[a] })#l, Kind].map{ _ => KiStar(att)}
      case TyVar(id, att)  => lookUpKind(id, st, dic, cm, att)
      case TyQVar(id, att) => lookUpKind(id, st, dic, cm, att)
      case TyAbs(s, k, t2, att) =>
            kindOf(t2, st, dic, cm, k :: idx).map { KiArrow(k, _, att) }
      case TyApp(f, x, att) => 
        kindOf(x, st, dic, cm, idx).flatMap { xx =>
        kindOf(f, st, dic, cm, idx).flatMap { ff =>
          ff match {
            case KiArrow(kf, kx, att2) =>
              if (kf == xx) kx.successNel[Error]
              else KindError("Argument kind does not match " + kf + " != " + xx + " in " + TyApp(f, x), att).failNel[Kind]
            case _ => KindError("Expected type abstraction, but found \n" + f + " in\n" +t, att).failNel[Kind]
          }
        }
      }
      case TyIndex(i, id, att) =>
        if (i >= 0 && i < idx.size) idx(i).successNel[Error]
        else 
        InternalError("Missind type index entry " + i + " in " + t, att).failNel[Kind]
      case TyMu(t1, t2, att) => kindOf(t1, st, dic, cm, idx).flatMap { tt1 =>
        kindOf(t2, st, dic, cm, idx).flatMap { tt2 =>
          tt1 match {
            case KiArrow(KiArrow(x1, KiStar(_), _), KiArrow(x2, KiStar(_), _), _) if (x1 == tt2 && tt2 == x2) =>
              KiStar().successNel[Error]
            case x =>
              KindError("Expected: " + KiArrow(KiArrow(tt2, KiStar()), KiArrow(tt2, KiStar())) +
                " in mu type,  but found " + x, att).failNel[Kind]
          }
        }
      }
      case TyPair(a, b, att) =>
        for {
          aa <- kindOf(a, st, dic, cm, idx)
          bb <- kindOf(b, st, dic, cm, idx)
        } yield KiPair(aa, bb)
      case TyFst(t, att) =>
        kindOf(t, st, dic, cm, idx).flatMap { tt =>
          tt match {
            case KiPair(a, b, att) => a.successNel[Error]
            case _                 => KindError("Selection only works on pairs, not on: " + t, att).failNel[Kind]
          }
        }
      case TySnd(t, att) =>
        kindOf(t, st, dic, cm, idx).flatMap { tt =>
          tt match {
            case KiPair(a, b, att) => b.successNel[Error]
            case _                 => KindError("Selection only works on pairs, not on: " + t, att).failNel[Kind]
          }
        }
       case x => InternalError(
         "Found unexpected type in kindOf, either TyUni or TyAmb. Both should not reach this code.",
         x.att).failNel[Kind]
    }
  }
}
