
package de.tuberlin.uebb.smith.modules

/**
 * Pretty printer for definitions and expressions.
 */
object SyntaxPrettyPrinter extends org.kiama.output.PrettyPrinter {

  def pretty(t: Any): String = t match {
    case e: Type => super.pretty(showType(e))
    case e: Kind => super.pretty(showKind(e))
    case e: Expr => super.pretty(showExpr(e))
    case e: Import => super.pretty(showImport(e))
    case e: Def => super.pretty(showDef(e))
    case e       => pretty_any(e)
  }

  implicit def showExpr(t: Expr): Doc =
    t match {
      case Bool(b: Boolean, att)                     => value(b)
      case TApp(l, r, att)                           => l <+> "[" <> r <> "]"
      case TAbs(id: String, k: Kind, e, att)         => parens("λ" <> id.toString <> "::" <> k <> "." <+> e)
      case Fold(t: Type, e: Expr, att)               => "FOLD" <> "[" <> t <> "]" <+> e
      case Unfold(t: Type, e: Expr, att)             => "UNFOLD" <> "[" <> t <> "]" <+> e
      case CaseOf(e: Expr, cases: List[Branch], att) => "case" <+> e <+> "branches"
      case Selection(e: Expr, s: String, att)        => e <> "!" <> s
      case Ascription(e: Expr, t: Type, att)         => e <+> ":" <+> t
      case LetSeq(s, b: List[Binding], e: Expr, att) => "LetSeq" <> nest(line <> list(b, "", showBinding)) <@> "IN" <> nest(line <> e)
      case LetPar(s, b: List[Binding], e: Expr, att) => "LetPar" <> nest(line <> list(b, "", showBinding)) <@> "IN" <> nest(line <> e)
      case v@Variant(s, e, t, att)                     => "<"<+>s<>"="<>e<+>">" //<+> ":" <+> t
      case Record(l, att)                            => l.mkString("{", ", ", "}")
      case Cond(c, t, e, att) => "if" <+> showExpr(c) <@> "then" <+>
        nest(line <> showExpr(t)) <@> "else" <> nest(line <> showExpr(e))
      case Abs(x, t, e, att)  => parens("λ" <>x.toString<> ":" <> showType(t) <> "." <> nest(line <> showExpr(e)))
      case Closure(x, t, e, dic, local, att)  => parens("λ" <>x.toString<> ":" <> showType(t) <> "." <> nest(line <> showExpr(e)))
      case App(f, e, att)     => parens(showExpr(f) <+> showExpr(e))
      case Add(x, y, att)     => x <+> "+" <+> y
      case Sub(x, y, att)     => x <+> "-" <+> y
      case Mul(x, y, att)     => x <+> "*" <+> y
      case Div(x, y, att)     => x <+> "/" <+> y
      case CharOf(x, y, att)  => x <> "." <> y
      case Concat(x, y, att)  => x <+> "+" <+> y
      case Greater(x, y, att) => x <+> ">" <+> y
      case StrSize(s, att)    => "SIZE" <+> s
      case Var(i, att)        => i.toString
      case QVar(i, att)       => i.toString
      case Num(v, att)        => value(v)
      case Str(s, att)        => dquotes(value(s))
      case ExprAmb(a, b, att) => parens(a <+> "|" <+> b)
      case e                  => pretty_any(e)
    }

  def showDef(d: Def) : Doc = d match {
    case ValDef(v, n, e, syn, att) => "VAL" <+> n.toString <+> "=" <>nest(line <> e) 
    case TypeDef(v, n, e, syn, att) => "TYPE" <+> n.toString <+> "=" <>nest(line <> e) 
    case BinderDef(v,bt, name, u, w, e , syn, att) => "BINDER" <+> name.toString <+> "=" <> nest(line <> e)

    case BindingDef(v, name, u, w, f, e , syn, att) => "BINDING" <+> name.toString <+> "=" <> nest(line <> e)
    case BracketDef(v, name, l, r, att) => "BRACKET " <+> name.toString
  }

  implicit def showVisibility(v: Visibility) : Doc = v match {
    case Public => "PUBLIC"
    case Private => "PRIVATE"
  }
  
  implicit def showImport(i: Import) : Doc = i match {
    case Import(v, im, mods, att) => v <+> "IMPORT" <+> im <+> sep(mods.map{showMod})
  }

  implicit def showImportedModule(im: ImportedModule) : Doc = im match {
    case NotQualified(i)    => i.toString
    case Qualified(i)       => "QUALIFIED" <+> i.toString
    case QualifiedAs(i, as) => "QUALIFIED" <+> i.toString <+> "AS" <+> as
  }

  def showMod(mod : ImportModification) : Doc = mod match {
    case Without(wos) => "WITHOUT" <+> list(wos, "")
    case Only(os)     => "ONLY" <+> list(os, "")
    case ImportRenaming(ren) => "RENAMING" <+> list(ren, "")
  }


  implicit def showBinding(t: Binding): Doc = t match {
    case BindingN(a, b, e, att)     => a.toString <+> "<" + b.toString + "> =" <+> e
    case BindingAs(a, b, t, e, att) => a.toString <+> "<" + b.toString + ">:" <+> t <+> "=" <+> e
    case BindingAmb(a, b)      => parens(a <+> "|" <+> b)
  }

  implicit def showType(t: Type): Doc =
    if (t.name != "") t.name
    else
      t match {
        case UnknownType                     => "⊥"
        case TyArrow(l: Type, r: Type, att) => "(" <> (showType(l) <+> "-->" <+> showType(r)) <> ")"
        case TyApp(l: Type, r: Type, att) => showType(l) <+>  showType(r)
        case TyForall(id, k: Kind, t: Type, att) => parens("∀" <> id.toString <> "::" <> k <> "." <+> showType(t))
        case TyAbs(id, k: Kind, t: Type, att) => parens("λ" <> id.toString <> "::" <> k <> "." <+> showType(t))
        case TyInt(att)                           => "Int"
        case TyString(att)                        => "String"
        case TyBool(att)                          => "Bool"
        case TyVar(id: Identifier, att) => "" <> id.toString
        case TyUni(id: Identifier, att) => "U" <> id.toString
        case TyQVar(id: Identifier, att) => "" <> id.toString
        case TyRecord(l: Map[String, Type], att) => l.mkString("{", ", ", "}")
        case TyVariant(l: Map[String, Type], att) => l.mkString("<", ", ", ">")
        case TyMu(l: Type, r: Type, att) => "MU" <> parens(showType(l) <+> "," <+> showType(r))
        case TyIndex(i: Int, id: Identifier, att) => "" <> id.toString  <> "|" <> (value(i))
        case TyPair(l, r, att) => parens(l <+> "," <+> r)
        case TyFst(k, att) => k <> ".1"
        case TySnd(k, att) => k <> ".2"
        case e                               => pretty_any(e)
      }

  implicit def showKind(k: Kind): Doc = k match {
    case KiStar(_)        => "*"
    case KiArrow(l, r, _) => parens(l <+> "=>" <+> r)
    case KiPair(l, r, _)  => "{"<>l <+> "," <+> r<>"}"
  }

  def catList(l: List[Doc], sep: Doc): Doc = (group(nest(lsep(l, sep))))
}
