package de.tuberlin.uebb.smith.modules

trait DefaultProrities {
  self: SDFGenerator => 

  def termDefaultPriorities(syn: List[AppSyntaxElement], s: String): Unit = {
    val (hs, ls) = termDefaults.splitAt(defaultTermPriorityLevel)
    val hhs = hs.map { case (dl, ds) => Higher(dl, ds, syn, s, true) }
    val lls = ls.map { case (dl, ds) => Higher(syn, s, dl, ds, true) }
    currentPriority = hhs ++ lls ++ currentPriority
  }
  val defaultTypePriorityLevel = 5
  def typeDefaultPriorities(syn: List[AppSyntaxElement], s: String): Unit = {
    val (hs, ls) = typeDefaults.splitAt(defaultTypePriorityLevel)
    val hhs = hs.map { case (dl, ds) => Higher(dl, ds, syn, s, true) }
    val lls = ls.map { case (dl, ds) => Higher(syn, s, dl, ds, true) }
    currentPriority = hhs ++ lls ++ currentPriority
  }

  val defaultTermPriorityLevel = 12
  val termDefaults: List[(List[AppSyntaxElement], String)] =
    List(
        (List(AppArg, Lexeme("!"), AppArg),                                                                  """Term "!" ID         -> Term"""), 
        (List(AppArg, Lexeme("["), AppArg, Lexeme("]")),                                                     """Term "[" Type "]"   -> Term"""), 
        (List(AppArg, AppArg),                                                                               """Term Term           -> Term"""), 
        (List(AppArg, Lexeme(":"), AppArg),                                                                  """Term ":" Type       -> Term"""),
        (List(Lexeme("MUL"), AppArg, AppArg),                                                                """ "MUL" Term Term    -> Term """), 
        (List(Lexeme("DIV"), AppArg, AppArg),                                                                """ "DIV" Term Term    -> Term """), 
        (List(Lexeme("ADD"), AppArg, AppArg),                                                                """ "ADD" Term Term    -> Term """), 
        (List(Lexeme("SUB"), AppArg, AppArg),                                                                """ "SUB" Term Term    -> Term """), 
        (List(Lexeme("CHAROF"), AppArg, AppArg),                                                             """ "CHAROF" Term Term -> Term """), 
        (List(Lexeme("CONCAT"), AppArg, AppArg),                                                             """ "CONCAT" Term Term -> Term """), 
        (List(Lexeme("GT"), AppArg, AppArg),                                                                 """ "GT" Term Term     -> Term """), 
        (List(Lexeme("SIZE"), AppArg, AppArg),                                                               """ "SIZE" Term        -> Term """), //here comes the custom syntax
        (List(Lexeme("FOLD"), Lexeme("["), AppArg, Lexeme("]"), AppArg),                                     """"FOLD" "[" Type "]" Term                  -> Term"""),
        (List(Lexeme("UNFOLD"), Lexeme("["), AppArg, Lexeme("]"), AppArg),                                   """"UNFOLD" "[" Type "]" Term                -> Term"""),
        (List(Lexeme("\\"), AppArg, Lexeme(":"), AppArg, Lexeme("."), AppArg),                               """"\" ID ":" Type "." Term                 -> Term"""), 
        (List(Lexeme("LET"), Lexeme("SEQ"), Lexeme("<"), AppArg, Lexeme(">"), AppArg, Lexeme("IN"), AppArg), """"LET" "SEQ" "<" Id ">" Binding+ "IN" Term -> Term"""), 
        (List(Lexeme("LET"), Lexeme("PAR"), Lexeme("<"), AppArg, Lexeme(">"), AppArg, Lexeme("IN"), AppArg), """"LET" "PAR" "<" Id ">" Binding+ "IN" Term -> Term"""), 
        (List(Lexeme("CASE"), AppArg, Lexeme("OF"), AppArg),                                                 """"CASE" Term "OF" { Branch ";" }+          -> Term"""), 
        (List(Lexeme("IF"), AppArg, Lexeme("THEN"), AppArg, Lexeme("ELSE"), AppArg),                         """"IF" Term "THEN" Term "ELSE" Term         -> Term"""), 
        (List(Lexeme("<"), AppArg, Lexeme("="), AppArg, Lexeme(">"), Lexeme("=>"), AppArg),                  """"<" ID "=" ID ">" "=>" Term               -> Branch"""))

  val typeDefaults: List[(List[AppSyntaxElement], String)] =
    List(
        (List(AppArg, AppArg),                                                      """Type Type                      -> Type"""), 
        (List(AppArg, Lexeme("."), Lexeme("1")),                                    """Type "." "1"                   -> Type"""),
        (List(AppArg, Lexeme("."), Lexeme("2")),                                    """Type "." "2"                   -> Type"""),
        (List(AppArg, Lexeme("->"), AppArg),                                        """Type "->" Type                 -> Type"""), 
        (List(AppArg, Lexeme("**"), AppArg),                                        """Type "**" Type                 -> Type"""), //here comes the custom syntax
        (List(Lexeme("FORALL"), AppArg, Lexeme("::"), AppArg, Lexeme("."), AppArg), """"FORALL" ID "::" Kind "." Type -> Type"""), 
        (List(Lexeme("\\"), AppArg, Lexeme("::"), AppArg, Lexeme("."), AppArg),     """ "\" ID "::" Kind "." Type    -> Type"""))

  val defaultSyntax = (termDefaults ++ typeDefaults).toMap
  val termDefaultsMap = termDefaults.toMap
  val typeDefaultsMap = typeDefaults.toMap
}
