package de.tuberlin.uebb.smith.modules
import org.kiama.output.PrettyPrinter
import scalaz._
import Scalaz._
import TypeSynonyms._
import Syntax._
import Unification._
import LookUp._
import TermRewriter._
trait TypeChecker extends TypeResult with KindChecker{

  def computeType(e: Expr, ctx: Context, dic: Dictionary, currentModule: Identifier): Result[Type] = {
    typeOf(e, ctx, dic, currentModule, Map(), Nil) &
      { x => checkType(x, ctx.st, dic, currentModule, Nil) } &
      { x => evalType(removeNamesFromType(x), ctx.st, dic, currentModule) } match {
        case Fail(t, e) => NonEmptyList(e.head, e.tail.toSeq: _*).fail[Type]
        case Ok(t)      => t.successNel[Error]
      }
  }

  //Binder 
  def checkTypeOfBinder(u: Type, w: Type, t1: Expr, ctx: Context, dic: Dictionary, currentModule: Identifier): Result[Type] = {
    for {
      uK <- kindOf(u, ctx.st, dic, currentModule, List())
      wK <- kindOf(w, ctx.st, dic, currentModule, List())
      _ <- if (uK == KiArrow(KiStar(), KiStar())) ().successNel[Error] else TypeError("Expected * => * for " + u + ", found " + uK, u.att).failNel[Type]
      _ <- if (wK == KiArrow(KiStar(), KiStar())) ().successNel[Error] else TypeError("Expected * => * for " + w + ", found " + wK, w.att).failNel[Type]
      T1 <- computeType(t1, ctx, dic, currentModule)
      eT1 <- evalTypeV(T1, ctx.st, dic, currentModule)
      eu <- evalTypeV(u, ctx.st, dic, currentModule)
      ew <- evalTypeV(w, ctx.st, dic, currentModule)
      eres <- evalTypeV(TyForall(new ShowId("X"), KiStar(), TyArrow(TyApp(eu, TyIndex(0)), TyApp(ew, TyIndex(0)))), ctx.st, dic, currentModule)
      _ <- if (eT1 &= eres)
        ().successNel[Error]
      else TypeError("Binder: Expected \n\t" + eres + ", but found \n\t" + T1, t1.att).failNel[Type]
    } yield eT1
  }

  def checkTypeOfSeqBinding(name: Identifier, u: Type, w: Type, v: Type, r: Type, tm: Expr, ctx: Context, dic: Dictionary, currentModule: Identifier): Result[Type] = {
    for {
      vK <- kindOf(v, ctx.st, dic, currentModule, List())
      rK <- kindOf(r, ctx.st, dic, currentModule, List())
      _ <- if (vK == KiArrow(KiStar(), KiStar())) ().successNel[Error] else TypeError("Binding "+ name + ": Expected * => * for " + v + ", found " + vK, v.att).failNel[Type]
      _ <- if (rK == KiArrow(KiStar(), KiStar())) ().successNel[Error] else TypeError("Binding "+ name +" : Expected * => * for " + r + ", found " + rK, r.att).failNel[Type] 
      Tm <- computeType(tm, ctx, dic, currentModule)
      eTm <- evalTypeV(Tm, ctx.st, dic, currentModule)
      er <- evalTypeV(r, ctx.st, dic, currentModule)
      ev <- evalTypeV(v, ctx.st, dic, currentModule)
      ew <- evalTypeV(w, ctx.st, dic, currentModule)
      eres <- evalTypeV(
        removeNamesFromType(
          TyForall("X", KiStar(), TyForall("Y", KiStar(),
            TyArrow(TyApp(ev, TyVar("X")),
              TyArrow(TyArrow(TyApp(er, TyVar("X")), TyApp(ew, TyVar("Y"))),
                TyApp(ew, TyVar("Y"))))))),
        ctx.st, dic, currentModule)
      _ <- if (eTm &= eres)
        ().successNel[Error]
      else TypeError("Binding " + name + ": Expected \n\t" + eres + ", but found \n\t" + Tm, tm.att).failNel[Type]
    } yield Tm
  }

  def checkTypeOfParBinding(name: Identifier, u: Type, w: Type, v: Type, r: Type, tm: Expr, ctx: Context, dic: Dictionary, currentModule: Identifier): Result[Type] = {
    for {
      vK <- kindOf(v, ctx.st, dic, currentModule, List())
      rK <- kindOf(r, ctx.st, dic, currentModule, List())
      _ <- if (vK == KiArrow(KiStar(), KiStar())) ().successNel[Error] else TypeError("Binding "+ name + ": Expected * => * for " + v + ", found " + vK, r.att).failNel[Type]
      _ <- if (rK == KiArrow(KiStar(), KiStar())) ().successNel[Error] else TypeError("Binding "+ name + ": Expected * => * for " + r + ", found " + rK, r.att).failNel[Type] 
      Tm <- computeType(tm, ctx, dic, currentModule)
      eTm <- evalTypeV(Tm, ctx.st, dic, currentModule)
      er <- evalTypeV(r, ctx.st, dic, currentModule)
      ev <- evalTypeV(v, ctx.st, dic, currentModule)
      ew <- evalTypeV(w, ctx.st, dic, currentModule)
      eres <- evalTypeV(
        removeNamesFromType(
          TyForall("X", KiStar(), TyForall("Y", KiStar(),
            TyArrow(TyApp(ev, TyVar("X")),
              TyApp(er, TyVar("X")))))),
        ctx.st, dic, currentModule)
      _ <- if (eTm &= eres)
        ().successNel[Error]
      else TypeError("Binding " + name + ": Expected \n\t" + eres + ", but found \n\t" + Tm, tm.att).failNel[Type]
    } yield Tm
  }

  def typeOf(e: Expr, ctx: Context, dic: Dictionary, cm: Identifier, tidx: Map[Identifier, Type], kidx: List[Kind]): TypeResult = {
    val ty = (e match {
      case Num(n, att)           => lift(TyInt(att))
      case Str(s: String, att)   => lift(TyString(att))
      case Bool(b: Boolean, att) => lift(TyBool(att))
      case Add(x, y, att) => typeOf(x, ctx, dic, cm, tidx, kidx) & { xx =>
        typeOf(y, ctx, dic, cm, tidx, kidx) & { yy =>
          if (xx &= TyInt()) {
            if (yy &= TyInt())
              lift(TyInt(att))
            else error(att, "Expected number, not: " + yy)
          } else error(att, "Expected number, not: " + xx)
        }
      }
      case Sub(x, y, att) => typeOf(x, ctx, dic, cm, tidx, kidx) & { xx =>
        typeOf(y, ctx, dic, cm, tidx, kidx) & { yy =>
          if (xx &= TyInt()) {
            if (yy &= TyInt())
              lift(TyInt(att))
            else error(att, "Expected number, not: " + yy)
          } else error(att, "Expected number, not: " + xx)
        }
      }
      case Mul(x, y, att) => typeOf(x, ctx, dic, cm, tidx, kidx) & { xx =>
        typeOf(y, ctx, dic, cm, tidx, kidx) & { yy =>
          if (xx &= TyInt()) {
            if (yy &= TyInt())
              lift(TyInt(att))
            else error(att, "Expected number, not: " + yy)
          } else error(att, "Expected number, not: " + xx)
        }
      }
      case Div(x, y, att) => typeOf(x, ctx, dic, cm, tidx, kidx) & { xx =>
        typeOf(y, ctx, dic, cm, tidx, kidx) & { yy =>
          if (xx &= TyInt()) {
            if (yy &= TyInt())
              lift(TyInt(att))
            else error(att, "Expected number, not: " + yy)
          } else error(att ,"Expected number, not: " + xx)
        }
      }
      case Greater(x, y, att) => typeOf(x, ctx, dic, cm, tidx, kidx) & { xx =>
        typeOf(y, ctx, dic, cm, tidx, kidx) & { yy =>
          if (xx &= TyInt()) {
            if (yy &= TyInt())
              lift(TyBool(att))
            else error(att, "Expected number, not: " + yy)
          } else error(att, "Expected number, not: " + xx)
        }
      }
      case Concat(x, y, att) => typeOf(x, ctx, dic, cm, tidx, kidx) & { xx =>
        typeOf(y, ctx, dic, cm, tidx, kidx) & { yy =>
          if (xx &= TyString()) {
            if (yy &= TyString())
              lift(TyString(att))
            else error(att, "Expected string, not: " + yy)
          } else error(att, "Expected string, not: " + xx)
        }
      }
      case CharOf(x, y, att) => typeOf(x, ctx, dic, cm, tidx, kidx) & { xx =>
        typeOf(y, ctx, dic, cm, tidx, kidx) & { yy =>
          if (xx &= TyString()) {
            if (yy &= TyInt())
              lift(TyString(att))
            else error(att, "Expected number, not: " + yy)
          } else error(att, "Expected string, not: " + xx)
        }
      }
      case StrSize(x, att) =>
        typeOf(x, ctx, dic, cm, tidx, kidx) & { xx =>
          if (xx &= TyString())
            lift(TyInt(att))
          else error(att, "Expected string, not: " + xx)
        }
      case App(f: Expr, x: Expr, att) =>
        typeOf(f, ctx, dic, cm, tidx, kidx) & { ft =>
          typeOf(x, ctx, dic, cm, tidx, kidx) & { xt =>
            evalType(ft, ctx.st, dic, cm) & { ff =>
              evalType(xt, ctx.st, dic, cm) & { xx =>
                ff match {
                  case TyArrow(arg, res, att2) =>
                    if (!(arg &= xx)) error(att, "Argument mismatch: \n\t" + arg + " != \n\t" + xx + " in " + e) 
                    else lift(res)
                  case UnknownType => lift(UnknownType)
                  case _           => error(att, "Expected function type, not: " + ff + " in " + f)
                }
              }
            }
          }
        }
      case Var(id, att)  => 
        if (tidx.contains(id)) lift(tidx(id))
      else 
        lookup(id, ctx.st, dic, cm, att)
      case QVar(id, att) => lookup(id, ctx.st, dic, cm, att)
      case Abs(x, t: Type, e: Expr, att) =>
        evalType(t, ctx.st, dic, cm) & { tv =>
          val ntidx = tidx + (x -> tv);
          typeOf(e, ctx, dic, cm, ntidx, kidx) & { t1 =>
            lift(TyArrow(tv, t1, att))
          }
        }
      case Record(l: Map[String, Expr], att) =>
        (l.mapValues(typeOf(_, ctx, dic, cm, tidx, kidx))).partition(_._2.isInstanceOf[Fail]) match {
          case (ss, ts) =>
            if (ss.isEmpty)
              lift(TyRecord(for ((s, Ok(t)) <- ts) yield (s, t), att))
            else
              Fail(UnknownType, (for ((_, Fail(_, e)) <- ss) yield e).toList.flatten)
        }

      case Selection(e: Expr, s: String, att) =>
        typeOf(e, ctx, dic, cm, tidx, kidx) & { t =>
          t match {
            case TyRecord(l, att2) =>
              if (l.contains(s)) lift(l(s))
              else error(att, "Record does not contain member: " + s)
            case UnknownType => lift(UnknownType)
            case _           => error(att, "Expected record for selection, not: " + t)
          }
        }
      case Ascription(e: Expr, t: Type, att) =>
        typeOf(e, ctx, dic, cm, tidx, kidx) & { t2 =>
          evalType(t, ctx.st, dic, cm) & { tv =>
            if (tv &= t2) lift(t2)
            else error(att, "Expected " + tv + " not " + t2)
          }
        }
      case Variant(s, e, t, att) =>
        typeOf(e, ctx, dic, cm, tidx, kidx) & { te =>
          evalType(t, ctx.st, dic, cm) & { tv =>
            tv match {
              case TyVariant(l, att2) =>
                if (l.contains(s))
                  if (l(s) &= te)
                    lift(tv)
                  else
                    error(att, "Type of variant field " + s + "::" + l(s) + " doesn't match " + te)
                else error(att, "Variant " + tv + " doesn't have a field " + s)
              case UnknownType => continue
              case _           => error(att, "Expected variant, not: " + tv)
            }
          }
        }
      case CaseOf(e: Expr, cases: List[Branch], att) =>
        typeOf(e, ctx, dic, cm, tidx, kidx) & { t =>
          evalType(t, ctx.st, dic, cm) & { t =>
            t match {
              case TyVariant(l, att2) =>
                (if (l.size != cases.size)
                  continue //moved that to interpreter error(att, "Not all variants cases have a branch")
                else
                  continue) & (
                  if (false && l.keys != cases.map(_.a).toSet)
                    error(att, "Branches don't match all fields of variant " + l.keys + " != " + cases.map(_.a).toSet)
                  else
                    cases.map {
                      case Branch(a, b, e, att) =>
                        typeOf(e, ctx, dic, cm, tidx + (ID(b) -> l(a)), kidx)
                    }.reduce({
                      case (z, x) => z & { t1 =>
                        x & { t2 =>
                          if (t1 == t2) lift(t2)
                          else if (t1 &= t2) continue
                          else error(att, "Type of branches does not match: " + t1 + " != " + t2)
                        }
                      }
                    }: (TypeResult, TypeResult) => TypeResult))
              case UnknownType => continue
              case _           => error(att, "Expected variant, not: " + t)

            }
          }
        }
      case LetSeq(s, b: List[Binding], e: Expr, att) => {
        lookUpBinder(s, cm, ctx, dic, att).fold(l => error(att, "Could not find Binder: " + s),
          bd => {
            var ntidx = tidx 
            //sequenital visibility
            b.foldLeft(continue(): TypeResult)({
              case (t, bb) =>
                t & { nt  =>
                  typeOf(bb.e, ctx, dic, cm, ntidx, kidx) & { f =>
                    typeCheckLet(bb, s, f, ctx, dic, cm)
                  } & {ct => ntidx = ntidx + (bb.vari -> ct); 
                    lift(ct) } 
                }
            }) & { last =>
              typeOf(e, ctx, dic, cm, ntidx, kidx) & { t2 =>

                val uniV = TyUni("VAR_S")
                val ws = for {
                  eu <- evalTypeV(bd.u, ctx.st, dic, cm)
                  ew <- evalTypeV(bd.w, ctx.st, dic, cm)
                  eUVAR <- evalTypeV(TyApp(eu, uniV), ctx.st, dic, cm)
                  eT2 <- evalTypeV(t2, ctx.st, dic, cm)
                  uniS <- unify(eUVAR, eT2).successNel[Error]
                  _ <- if (uniS._1)
                    ().successNel[Error]
                  else TypeError("LET SEQ: Could not unify " + eUVAR + " with " + t2, att).failNel[Type]
                  S <- uniS._2(uniV).successNel[Error]
                  eWS <- evalTypeV(TyApp(ew, S), ctx.st, dic, cm)
                } yield eWS
                ws.fold(l => Fail(UnknownType, l.list), ws => Ok(ws))
              }
            }
          })
      }
      case LetPar(s, b: List[Binding], e: Expr, att) => {
        lookUpBinder(s, cm, ctx, dic, att).fold(l => error(att, "Could not find Binder: " + s),
          bd => {
            var ntidx = tidx
            b.foldLeft(continue(): TypeResult)({
              case (t, bb) =>
                t & { nt  =>
                  typeOf(bb.e, ctx, dic, cm, tidx, kidx) & { f =>
                    typeCheckLet(bb, s, f, ctx, dic, cm)
                  } & {ct => ntidx = ntidx + (bb.vari -> ct); 
                    lift(ct) } 
                }
            }) & { last =>
              //LetPar
              typeOf(e, ctx, dic, cm, ntidx, kidx) & { t2 =>

                val uniV = TyUni("VAR_S")
                val ws = for {
                  eu <- evalTypeV(bd.u, ctx.st, dic, cm)
                  ew <- evalTypeV(bd.w, ctx.st, dic, cm)
                  eUVAR <- evalTypeV(TyApp(eu, uniV), ctx.st, dic, cm)
                  eT2 <- evalTypeV(t2, ctx.st, dic, cm)
                  uniS <- unify(eUVAR, eT2).successNel[Error]
                  _ <- if (uniS._1)
                    ().successNel[Error]
                  else TypeError("LET PAR: Could not unify " + eUVAR + " with " + t2, att).failNel[Type]
                  S <- uniS._2(uniV).successNel[Error]
                  eWS <- evalTypeV(TyApp(ew, S), ctx.st, dic, cm)
                } yield eWS
                ws.fold(l => Fail(UnknownType, l.list), ws => Ok(ws))
              }
            }
          })
      }
      case Cond(c: Expr, t: Expr, e: Expr, att) =>
        typeOf(c, ctx, dic, cm, tidx, kidx) & { t1 =>
          typeOf(t, ctx, dic, cm, tidx, kidx) & { t2 =>
            typeOf(e, ctx, dic, cm, tidx, kidx) & { t3 =>
              if (t2 &= t3)
                if (t1 &= TyBool())
                  Ok(t3)
                else
                  error(c.att, "If condition is not Bool")
              else if (t1 &= TyBool())
                error(att, "Branches of condition don't match: \n\t" + t2 + " != \n\t" + t3)
              else
                error(att, "If condition is not Bool", "Branches of condition don't match: " + t2 + " != " + t3)

            }
          }
        }
      case TApp(f: Expr, x: Type, att) =>
        typeOf(f, ctx, dic, cm, tidx, kidx) & { ff =>
          ff match {
            case TyForall(s, k, t1, att) =>
              kindOf(x, ctx.st, dic, cm, kidx).fold(
                e => Fail(UnknownType, e.list),
                k2 =>
                  if (k == k2)
                    lift(substituteTopTypeAndShift[Type](x, t1))
                  else error(att, "Kind of " + x + "::" + k2 + " does not match " + k))
            case UnknownType => lift(UnknownType)
            case _           => error(att, "expected universal type, not: " + ff)
          }
        }
      case TAbs(x: String, k: Kind, e: Expr, att) => {
        typeOf(e, ctx, dic, cm, shift[Map[Identifier,Type]](tidx), k :: kidx) & { t1 =>
          lift(TyForall(x, k, t1))
        }
      }
      case Fold(t: Type, e: Expr, att) => typeOf(e, ctx, dic, cm, tidx, kidx) & { et =>
        checkType(et, ctx.st, dic, cm, kidx) & { S =>
          evalType(et, ctx.st, dic, cm) & { S =>
            checkType(t, ctx.st, dic, cm, kidx) & { tt =>
              evalType(t, ctx.st, dic, cm) & { tt =>
                kindOf(S, ctx.st, dic, cm, kidx).fold(
                  e => Fail(UnknownType, e.list),
                  k2 =>
                    tt match {
                      case TyMu(t1, t2, att) =>
                        checkType(TyApp(TyApp(t1, TyAbs("arg", k2, TyMu(shift(t1), TyIndex(0)))), t2), ctx.st, dic, cm, kidx) & { x =>
                          evalType(x, ctx.st, dic, cm) & { x =>
                            t.name = t.toString
                            val mu = TyMu(t1, t2)
                            mu.name = t.toString
                            if (x &= S)
                              lift(mu)
                            else error(att, "FOLD: "+ e + " has type : \n\t" + x + " does not match with \n\t" + S)
                          }
                        }
                      case e => error(att, "Expected TyMu in fold, not " + e)
                    })
              }
            }
          }
        }
      }

      case Unfold(t: Type, e: Expr, att) => typeOf(e, ctx, dic, cm, tidx, kidx) & { et =>
        checkType(et, ctx.st, dic, cm, kidx) & { S =>
          evalType(et, ctx.st, dic, cm) & { S =>
            checkType(t, ctx.st, dic, cm, kidx) & { tt =>
              evalType(t, ctx.st, dic, cm) & { tt =>
                tt match {
                  case TyMu(t1, t2, att) =>
                    kindOf(S, ctx.st, dic, cm, kidx).fold(
                      e => Fail(UnknownType, e.list),
                      { k2 =>
                        val mu = TyMu(shift(t1), TyIndex(0, new ShowId("arg")))
                        mu.name = t.toString
                        val conc = TyApp(TyApp(t1, TyAbs("arg", k2, mu)), t2)
                        lift(conc)
                      })
                  case e => error(att, "Expected TyMu in unfold, not " + e)

                }

              }
            }
          }
        }
      }

    }) &  { t => evalType(t, ctx.st, dic, cm) }
    ty
  }

  def typeCheckLetV(binding: Binding, binder: Identifier, t1: Type, ctx: Context, dic: Dictionary, cm: Identifier): Result[(Type, Type)] = {
    val uniV = TyUni("VAR_S")
    for {
      b <- lookUpBinding(binding.name, cm, ctx, dic, binding.att)
      _ <- if (b.forId == binder)
        ().successNel[Error]
      else TypeError("Binding " + binding + " does belong to " + b.forId + " not to " + binder, binding.att).failNel[Type]
      er <- evalTypeV(b.r, ctx.st, dic, cm)
      ev <- evalTypeV(b.v, ctx.st, dic, cm)
      eUVAR <- evalTypeV(TyApp(ev, uniV), ctx.st, dic, cm)
      eT1 <- evalTypeV(t1, ctx.st, dic, cm)
      uniS <- unify(eUVAR, eT1).successNel[Error]
      _ <- if (uniS._1)
        ().successNel[Error]
      else TypeError("LET: Could not unify " + eUVAR + " with " + t1, binding.att).failNel[Type] 
      S1 <- (if (eUVAR == eT1) eT1//there was nothing to unify, e.g. a constant function \X::*.Bool
             else uniS._2(uniV)).successNel[Error]
      eRS <- evalTypeV(TyApp(er, S1), ctx.st, dic, cm)
      _ <- if(binding.isInstanceOf[BindingAs])
            {
              val bas = binding.asInstanceOf[BindingAs]
              evalTypeV(bas.t, ctx.st, dic, cm).flatMap{bt =>
              if (eRS == bt)
                ().successNel[Error]
              else TypeError("Binding ascription expected " + bt + " but found " +eRS, binding.att).failNel[Type]
              }
            }
            else ().successNel[Error]
    } yield (eRS,S1)


  }
  def typeCheckLet(binding: Binding, binder: Identifier, t1: Type, ctx: Context, dic: Dictionary, cm: Identifier): TypeResult =
    typeCheckLetV(binding, binder, t1, ctx, dic, cm).fold( l => Fail(UnknownType, l.list) ,
      {case (rs, s1) => lift(rs)})

  def lookup(id: Identifier, st: SymbolTable, dic: Dictionary, cm: Identifier, att: Attribute): TypeResult =
    (dic(id,att), dic(cm ~ id, att)) match {
      case (Success(rid), _) =>
        if (st.exprTypes.contains(rid)) lift(st.exprTypes(rid))
        else error(att, "Should not happen! Unknown Identifier: " + id + "/" + rid + "***" + st)
      case (_, Success(rid)) => //if (st.exprTypes.contains(rid)) lift(st.exprTypes(rid)) else 
        error(att, "Should not happen! Unknown Identifier: " + id)
      case _                 => Fail(UnknownType, List(ContextError("Unknown Identifier: " + id, att)))
    }

}
