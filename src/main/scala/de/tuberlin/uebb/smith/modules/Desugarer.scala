package de.tuberlin.uebb.smith.modules

import TypeSynonyms._
import scalaz._
import Scalaz._
import Syntax._
import LookUp._
import Unification._
import TermRewriter._

trait Desugarer extends TypeChecker {


  def desugarExpr(e: Expr, currentModule : Identifier, ctx : Context, dic: Dictionary) : Result[Expr] =
    try {
         desugarExpr2(e, currentModule, ctx, dic, Map(), Nil).successNel[Error]
    } catch {
      //since we use kiama we have to wrapper our errors and throw them
      //here we unwrap them and return them if necessary.
      //normally this should not append at this point 
      case f : ExceptionWrapper[Expr] @unchecked => f.e
      case FailWrapper(Fail(t, e)) => NonEmptyList(e.head, e.tail.toSeq: _*).fail[Expr]
    }


  def desugarExpr2(e: Expr, currentModule : Identifier, ctx : Context, dic: Dictionary, idx: Map[Identifier, Type], kidx: List[Kind]) : Expr =
        desugarRule(currentModule, ctx, dic, idx, kidx)(e).get.asInstanceOf[Expr]

  def desugarRule(cm: Identifier, ctx: Context,
    dic: Dictionary, idx: Map[Identifier,Type], kidx: List[Kind]) : Strategy = rule {
    case Abs(x, t: Type, e: Expr, att) => Abs(x, t, desugarExpr2(e, cm, ctx, dic, idx + (x -> t), kidx), att)
    case CaseOf(e: Expr, cases: List[Branch], att) =>
      var bs = List[Branch]()
        (typeOf(e, ctx, dic, cm, idx, kidx) & { (t :Type) =>
          evalType(t, ctx.st, dic, cm) & { t =>
            t match {
              case TyVariant(l, att2) =>
                bs = cases.map {
                  case Branch(a, b, e, att) =>
                    Branch(a, b, desugarExpr2(e, cm, ctx, dic,  idx + (ID(b) -> l(a)), kidx))
                }
                continue()
              case _ => continue()
            }}}).$$
      CaseOf(desugarExpr2(e, cm, ctx, dic,  idx, kidx), bs, att)
    case TAbs(x, k, e, att) => TAbs(x, k, desugarExpr2(e, cm, ctx, dic, idx, k :: kidx), att)
    case ls: LetSeq => desugarExpr2(ExceptionWrapper(desugarLetSeq(ls, cm, ctx, dic, idx, kidx)),cm, ctx, dic, idx, kidx)
    case lp: LetPar => desugarExpr2(ExceptionWrapper(desugarLetPar(lp, cm, ctx, dic, idx, kidx)),cm, ctx, dic, idx, kidx)
    //for everything else we don't have to collect type information, so we can handle it in one case
    case x          => all(desugarRule(cm, ctx, dic, idx, kidx))(x).get
  }


  def desugarLetPar(ls: LetPar, cm : Identifier, ctx : Context, dic : Dictionary, idx: Map[Identifier, Type], kidx: List[Kind]): Result[Expr] = ls match {
    case LetPar(s, bis: List[Binding], e: Expr, att) => 
      lookUpBinder(s, cm, ctx, dic, e.att).fold(l => InternalError("Could not find Binder: " + s, e.att).failNel[Expr],
        { bd =>
          val uniV = TyUni("VAR_S")
          val ws = typeOf(ls, ctx, dic, cm, idx, kidx).& { tt => evalType(tt, ctx.st, dic, cm) }.$$
          val t = ExceptionWrapper(evalTypeV(TyApp(bd.w,uniV), ctx.st, dic, cm))
          val (b,sub) = unify(ws, t) 
          if(!b) return InternalError("Could not unify " + ws + " with "+TyApp(bd.w,uniV), ls.att).failNel[Expr]
          val s = sub(uniV) 
          val ts = App(TApp(bd.e, s), e)
          val abs = buildAbs(bis, ts, s, ctx,cm, dic, idx, kidx)
          val res = applyBinding(bis, abs, s, ctx, cm, dic, idx, kidx).successNel[Error]
          res
    })
  }
  def applyBinding(bindings: List[Binding], ts: Expr, s: Type, ctx: Context, 
    cm: Identifier, dic: Dictionary, idx: Map[Identifier, Type], kidx: List[Kind]) : Expr = bindings match {
    case Nil => ts
    case bi::bs => {
      val binding = lookUpBinding(bi.name, cm, ctx, dic, bi.att).toOption.get // Error?
      val binder = lookUpBinder(binding.forId, cm, ctx, dic, bi.att).toOption.get // Error?
      val t1 = typeOf(bi.e, ctx, dic, cm, idx, kidx).$$
      val (rs1,s1) = typeCheckLetV(bi, binding.forId, t1, ctx, dic, cm).toOption.get
      val tres = App(ts,App(TApp(TApp(binding.e, s1), s), bi.e))
      applyBinding(bs, tres, s, ctx,  cm, dic, idx + (bi.vari -> rs1), kidx)
    }
  }
  def buildAbs(bindings: List[Binding], ts: Expr, s: Type, ctx: Context, 
    cm: Identifier, dic: Dictionary, idx: Map[Identifier, Type], kidx: List[Kind]) : Expr = bindings match {
    case Nil => ts
    case bi::bs => {
      val binding = lookUpBinding(bi.name, cm, ctx, dic, bi.att).toOption.get // Error?
      val binder = lookUpBinder(binding.forId, cm, ctx, dic, bi.att).toOption.get // Error?
      val t1 = typeOf(bi.e, ctx, dic, cm, idx, kidx).$$
      val (rs1,s1) = typeCheckLetV(bi, binding.forId, t1, ctx, dic, cm).toOption.get
      val tin = buildAbs(bs, ts, s, ctx,  cm, dic, idx + (bi.vari -> rs1), kidx)
      Abs(bi.vari, rs1, tin)
    }
  }

  def desugarLetSeq(ls: LetSeq, cm : Identifier, ctx : Context, dic : Dictionary, idx: Map[Identifier, Type], kidx: List[Kind]): Result[Expr] = ls match {
    case LetSeq(s, bis: List[Binding], e: Expr, att) => 
      lookUpBinder(s, cm, ctx, dic, e.att).fold(l => InternalError("Could not find Binder: " + s, e.att).failNel[Expr],
        { bd =>
          val uniV = TyUni("VAR_S")
          val ws = typeOf(ls, ctx, dic, cm, idx, kidx).& { tt => evalType(tt, ctx.st, dic, cm) }.$$
          val t = ExceptionWrapper(evalTypeV(TyApp(bd.w,uniV), ctx.st, dic, cm))
          val (b,sub) = unify(ws, t) 
          if(!b) return InternalError("Could not unify " + ws + " with "+TyApp(bd.w,uniV), ls.att).failNel[Expr]
          val s = sub(uniV) 
          val ts = App(TApp(bd.e, s), e)
          val res = desugarBindings(bis, ts, s, ctx, cm, dic, idx, kidx).successNel[Error]
          res
    })
  }

  def desugarBindings(bindings: List[Binding], ts: Expr, s: Type, ctx: Context, 
    cm: Identifier, dic: Dictionary, idx: Map[Identifier, Type], kidx: List[Kind]) : Expr = bindings match {
    case Nil => ts
    case bi::bs => {
      val binding = lookUpBinding(bi.name, cm, ctx, dic, bi.att).toOption.get // Error?
      val binder = lookUpBinder(binding.forId, cm, ctx, dic, bi.att).toOption.get // Error?
      val t1 = typeOf(bi.e, ctx, dic, cm, idx, kidx).$$
      val (rs1,s1) = typeCheckLetV(bi, binding.forId, t1, ctx, dic, cm).toOption.get
      val tres = desugarBindings(bs, ts, s, ctx,  cm, dic, idx + (bi.vari -> rs1), kidx)

      App(App(TApp(TApp(binding.e, s1), s), bi.e), Abs(bi.vari, rs1, tres))
    }
  }
}
