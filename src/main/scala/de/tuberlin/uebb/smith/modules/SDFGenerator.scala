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
// import scala.util._
import scala.collection.mutable.HashMap
trait SDFGenerator extends DefaultProrities{
  self: Parser => 

  var defaultSdfFile = "./SDF2/BinderLang.def"
  val tmp = System.getProperty("java.io.tmpdir")
  parseTableFile = tmp + "/Smith.tbl"
  var mergedSdfFile = tmp + "/Smith.def"

  /**
    * Represents whenever the current maps have the same content as the file on disk
    * true if that not the case, and we have to write it to disk before we can parse.
    */
  var dirty = false
  resetSDFFile()
  buildTable()

  def cleanUp() : Unit = {
    (new File(tmp + "/Smith.def")).delete
    (new File(tmp + "/Smith.tbl")).delete
  }
  def resetSDFFile(): Unit = {
    val src = new File(defaultSdfFile)
    val dest = new File(mergedSdfFile)
    new FileOutputStream(dest) getChannel () transferFrom (
      new FileInputStream(src) getChannel, 0, Long.MaxValue)
  }

  def clearSDF(): Unit =
    {
      syntaxValTable.clear
      syntaxTypeTable.clear
      syntaxBindingTable.clear
      syntaxBinderTable.clear
      syntaxStrings.clear
      currentProductions = List()
      currentPriority = List()
      resetSDFFile()
      buildTable()
    }

  def createCurrentTable() =
    {
      resetSDFFile()
      buildCurrent()
      buildTable()
    }

  def buildTable(): Unit = {
    try {
        val result = List("sdf2table", "-i", mergedSdfFile,
        "-o", parseTableFile, "-m", "BinderLang/Module").! == 0
    }
    catch {
      case e: Throwable => 
        cleanUp()
        println("Could not run sdf2table. Is it in the PATH env?")
        System.exit(0)
    }
    dirty = false
  }
  def appendToSdf(s: String) = {
    val fw = new FileWriter(mergedSdfFile, true)
    fw.write("\n" + escape(s) + "\n")
    fw.close()
    dirty = true
  }
  def escape(s:String) = s.replaceAll("""\\""", """\\\\""")

  def buildCurrent(): Unit = {
    val pro = currentProductions.mkString("context-free syntax\n", "\n", "")
    if (!currentProductions.isEmpty)
      appendToSdf(pro)
    val pri = currentPriority.mkString("context-free priorities\n", "\ncontext-free priorities\n", "")
    if (!currentPriority.isEmpty)
      appendToSdf(pri)
  }

  type MutableMap[A, B] = scala.collection.mutable.Map[A, B]

  /**
    * These map from a string to the information needed to parse the user syntax.
    * The string is the name used in the cons attribute in the SDF definition for that syntax.
    */
  val syntaxValTable: MutableMap[String, List[(Identifier, Type)]] = HashMap().withDefaultValue(Nil)
  val syntaxTypeTable: MutableMap[String, List[Identifier]] = HashMap().withDefaultValue(Nil)
  val syntaxBindingTable: MutableMap[String, List[(Identifier, BindeePosition)]] = HashMap().withDefaultValue(Nil)
  val syntaxBinderTable: MutableMap[String, List[(Identifier, List[BinderSyntaxElement], BinderType)]] = HashMap().withDefaultValue(Nil)
  //save all strings from syntax in this map
  //so we can look it up and use in bracket priority building
  //else we would have to look it up and rebuild knowing the type/kind
  val syntaxStrings: MutableMap[List[AppSyntaxElement], List[String]] = HashMap().withDefaultValue(Nil)

  var currentProductions: List[String] = List()
  var currentPriority: List[Priority] = List()
  abstract class Priority

  /**
    * Represents a SDF-Priority: left > right
    * If one of the sides is part of the core language, 
    * we use .> instead of > because that does not contribute to any transitive closure.
    * The transitive closure with default syntax creates problems, you don't see from the "outside".
    * take for example two operators : {{{ARG + ARG}}} and {{{ARG * ARG }}}
    * and {{{
    * BRACKET mp = ARG AS ARG + ( ARG * ARG )
    * BRACKET m = ARG AS  ARG ( ARG +  ARG ) }}}
    * We get the following priorities: {{{
    * ARG * ARG > ARG + ARG
    * ARG + ARG > ARG ARG }}}
    * The default priorities still include:
    * {{{ ARG ARG > ARG * ARG }}}
    * So we get implicit:
    * {{{ARG ARG > ARG * ARG > ARG + ARG > ARG ARG}}}
    * As a result neither f 2 + 3 nor f 2 * 3 will be accepted from the parser.
    * The only other solution, besides using .> for default priorities,
    * is tracking the transitive chain and change all affected priorities. 
    * But this implicit change would be hard to understand from the outside, too.
    * 
    */
  case class Higher(left: List[AppSyntaxElement], leftString: String, right: List[AppSyntaxElement], rightString: String, isDefault: Boolean = false) extends Priority {
    override def toString() = {
      val sign = if (isDefault) "\n.> " else  "\n> "
      leftString + sign + rightString
    }
  }

  case class PLeftAssociativity(syn: List[AppSyntaxElement], string: String) extends Priority {
    override def toString() = "{ left: " + string + "}"
  }
  case class PRightAssociativity(syn: List[AppSyntaxElement], string: String) extends Priority {
    override def toString() = "{ right: " + string + "}"
  }

  //we have to know 
  abstract class BindeePosition
  case object BindeeLeft extends BindeePosition //exm: BINDEE -> VAR
  case object BindeeRight extends BindeePosition //exm: VAR <- BINDEE
  case object BindeeAlone extends BindeePosition //exm: BINDEE ;

  def appendProduction(s: String) = {
    currentProductions = s :: currentProductions
    dirty = true
  }

  def appendPriority(p: Priority) = {
    currentPriority = p :: currentPriority
    dirty = true
  }

  /**
    * Takes the list of current priorities and filters the priority
    * which is affects x and y. Returns the list without this priority.
    */
  def removeDefault(x: List[AppSyntaxElement], y: List[AppSyntaxElement]) = {
    val r = currentPriority.filter { p =>
      p match {
        case Higher(hl, _, ll, _, _) if (hl == x && ll == y) || (hl == y && ll == x) => false
        case _ => true
      }}
      r
    }

  /**
    * Build imported syntax and priorities.
    */
  def buildImportedSyntax(dic: Dictionary, modules: Map[Identifier, Module], st: SymbolTable, att: Attribute): Result[Unit] =
    {
      val ids = dic.privateMap ++ dic.publicMap
      val r = ids.map({
        case (localId, globalId: QID) =>
          val (moduleId, name) = globalId.qid.splitAt(globalId.qid.size - 1)
          val module = modules(moduleId)
          if (module.valDef.contains(name))
          {
            val d = module.valDef(name)
            d.syn.map(buildProductionForVal(_, localId, st.exprTypes(globalId), att)).getOrElse(().successNel[Error])
          }
          else if (module.typeDef.contains(name))
          {
            val d = module.typeDef(name)
            d.syn.map(buildProductionForType(_, localId, st.typeKinds(globalId), att)).getOrElse(().successNel[Error])
          }
          else if (module.bindingDef.contains(name)){
            val bd = module.bindingDef(name)
            bd.syn.map(buildProductionForBinding(_, localId, att)).getOrElse(().successNel[Error])
          }
          else if (module.binderDef.contains(name)) {
            val bd = module.binderDef(name)
            bd.syn.map(buildProductionForBinder(bd.binderType, _, localId, att)).getOrElse(().successNel[Error])
          }
          else
            ().successNel[Error]
      }).toList
      //brackets has to come last, because its depends on the syntax from other defs to work
      val br = ids.map({
        case (localId, globalId: QID) =>
          val (moduleId, name) = globalId.qid.splitAt(globalId.qid.size - 1)
          val module = modules(moduleId)
          if (module.bracketDef.contains(name)) {
            buildPriority(module.bracketDef(name), att) match {
              case Success(_) => ().successNel[Error] 
              case Failure(e) => 
                (ContextError("Error with handling imported bracket def. "+
                              "Maybe the corresponding syntax was not imported?", att) <:: e).fail[Unit]
            }
          }
          else
            ().successNel[Error]
      }).toList

      (r++br).sequence[({ type l[a] = Result[a] })#l, Unit].map { x => () }
    }

  /**
    * Creates the SDF-Production for a VAL def
    * and appends it to the SDF definition
    */
  def buildProductionForVal(syn: List[AppSyntaxElement], name: Identifier, t: Type, att: Attribute): Result[Unit] = {
    if (defaultSyntax.contains(syn) || syn.count(_.isInstanceOf[Lexeme]) == 0)
      return SDFGeneratorError("Can not override core language", att).failNel[Unit]
    val synName = "<" + syn.toString + ">"
    val prod = appSyntaxToString(syn, t)
    syntaxValTable(synName) ::= (name, t)
    prod.map { p =>
      val s = p.mkString("", " ", "") + " -> Term"
      syntaxStrings(syn) ::= s
      termDefaultPriorities(syn, s)
      appendProduction(s + "{cons(\"" + synName + "\")" + "}")
    }
  }

  /**
    * Creates the SDF-Production for a TYPE def
    * and appends it to the SDF definition
    */
  def buildProductionForType(syn: List[AppSyntaxElement], name: Identifier, t: Kind, att: Attribute): Result[Unit] = {
    if (defaultSyntax.contains(syn) || syn.count(_.isInstanceOf[Lexeme]) == 0)
      return SDFGeneratorError("Can not override core language", att).failNel[Unit]
    val synName = "<" + syn.toString + ">"
    val prod = tappSyntaxToString(syn, t)
    syntaxTypeTable(synName) ::= name
    prod.map { p =>
      val s = p.mkString("", " ", "") + " -> Type"
      syntaxStrings(syn) ::= s
      typeDefaultPriorities(syn, s)
      appendProduction(s + "{cons(\"" + synName + "\")" + "}")
    }
  }

  /**
    * Creates the SDF-Production for a BINDER def
    * and appends it to the SDF definition
    */
  def buildProductionForBinding(syn: List[BindingSyntaxElement], name: Identifier, att: Attribute): Result[Unit] = {
    if (syn.count(x => x == BindingVar) > 1)
      return SDFGeneratorError("Syntax for " + name + " needs only one VAR", att).failNel[Unit]
    if (syn.count(x => x == BindingBindee) != 1)
      return SDFGeneratorError("Syntax for " + name + " needs excatly one BINDEE", att).failNel[Unit]
    val synName = syn.toString + "Syntax"
    val prod = bindingSyntaxToString(syn)
    val bpostion =
      if (syn.indexOf(BindingVar) == -1) BindeeAlone
      else if (syn.indexOf(BindingVar) < syn.indexOf(BindingBindee)) BindeeRight
      else BindeeLeft
    syntaxBindingTable(synName) ::= (name, bpostion)
    prod.map { p =>
      val s = p.mkString("", " ", "") ++ " -> Binding"
      syntaxStrings(convertToAppSyntax(syn)) ::= s
      appendProduction(s + "{cons(\"" + synName + "\")}")
    }
  }

  /**
    * Creates the SDF-Production for a BINDING def
    * and appends it to the SDF definition
    */
  def buildProductionForBinder(binderTyp: BinderType, syn: List[BinderSyntaxElement], name: Identifier, att : Attribute): Result[Unit] = {
    if (syn.count(x => x == BinderBody) != 1)
      return SDFGeneratorError("Syntax for " + name + " needs excatly one BODY", att).failNel[Unit]
    if (syn.count(x => !x.isInstanceOf[Lexeme]) != 2)
      return SDFGeneratorError("Syntax for " + name + " needs excatly one BINDING", att).failNel[Unit]
    val synName = syn + "Syntax"
    val prod = binderSyntaxToString(syn)
    syntaxBinderTable(synName) ::= (name, syn, binderTyp)
    prod.map { p =>
      val s = p.mkString("", " ", "") ++ " -> Term"
      syntaxStrings(convertToAppSyntax(syn)) ::= s
      termDefaultPriorities(convertToAppSyntax(syn), s)
      appendProduction(s + "{cons(\"" + synName + "\")}")
    }
  }

  def convertToAppSyntax(l: List[SyntaxElement]): List[AppSyntaxElement] = l.map {
    case l @ Lexeme(_) => l
    case _             => AppArg
  }.asInstanceOf[List[AppSyntaxElement]]

  def bindingSyntaxToString(a: List[BindingSyntaxElement]): Result[List[String]] = a match {
    case Lexeme(l) :: as     => bindingSyntaxToString(as).map { r => "\"" + l + "\"" :: r }
    case BindingVar :: as    => bindingSyntaxToString(as).map { r => "ID" :: r }
    case BindingBindee :: as => bindingSyntaxToString(as).map { r => "Term" :: r }
    case Nil                 => Nil.successNel[Error]
    case _                   => InternalError("Only here to handle warnings", EmptyAttribute).failNel[List[String]]
  }

  def binderSyntaxToString(a: List[BinderSyntaxElement]): Result[List[String]] = a match {
    case Lexeme(l) :: as  => binderSyntaxToString(as).map { r => "\"" + l + "\"" :: r }
    case BinderBody :: as => binderSyntaxToString(as).map { r => "Term" :: r }
    case x :: as          => binderSyntaxToString(as).map { r => "Binding+" :: r }
    case Nil              => Nil.successNel[Error]
  }

  /**
   * Converts a List of AppSyntaxElements to a List of SDF Strings
   */
  def appSyntaxToString(a: List[AppSyntaxElement], t: Type): Result[List[String]] = (a, t) match {
    case (Lexeme(l) :: as, _)                   => appSyntaxToString(as, t).map { r => "\"" + l + "\"" :: r }
    case (AppArg :: as, TyArrow(l, r, att))     => appSyntaxToString(as, r).map { r => "Term" :: r }
    case (AppArg :: as, TyForall(_, _, r, att)) => appSyntaxToString(as, r).map { r => "Type" :: r }
    case (AppArg :: as, _)                      => SDFGeneratorError("Expected function type or forall type to use with SYNTAX, but found: " + t, t.att).failNel[List[String]]
    case (Nil, _)                               => Nil.successNel[Error]
    case _                                => InternalError("Only here to handle warnings", EmptyAttribute).failNel[List[String]]
  }
  def tappSyntaxToString(a: List[AppSyntaxElement], k: Kind): Result[List[String]] = (a, k) match {
    case (Lexeme(l) :: as, _)             => tappSyntaxToString(as, k).map { r => "\"" + l + "\"" :: r }
    case (AppArg :: as, KiArrow(_, r, _)) => tappSyntaxToString(as, r).map { r => "Type" :: r }
    case (AppArg :: as, _)                => SDFGeneratorError("Expected typeoperator to use with SYNTAX, but found: " + k, k.att).failNel[List[String]]
    case (Nil, _)                         => Nil.successNel[Error]
    case _                                => InternalError("Only here to handle warnings", EmptyAttribute).failNel[List[String]]
  }

  def buildPriority(bd: BracketDef, att: Attribute): Result[Unit] = {
    //get delimiter
    val (leftDelimiter, rightDelimiter) =
      bd.r.bracket.map(x => (x.l, x.r)).getOrElse((Lexeme("("), Lexeme(")")))
    val rhs = bd.r.l
    if (leftDelimiter == rightDelimiter)
      return SDFGeneratorError("Left and right delimiter are not distinguishable", att).failNel[Unit]

    if (rhs.count(x => x == leftDelimiter) != 1)
      return SDFGeneratorError("Exactly one left delimiter allowed in right hand side of bracket. Found: " + rhs.count(x => x == leftDelimiter) + " in " + rhs, att).failNel[Unit]
    if (rhs.count(x => x == rightDelimiter) != 1)
      return SDFGeneratorError("Exactly one right delimiter allowed in right hand side of bracket. Found: " + rhs.count(x => x == rightDelimiter) + " in " + rhs, att).failNel[Unit]

    val leftPos = rhs.indexOf(leftDelimiter)
    val rightPos = rhs.indexOf(rightDelimiter)


    if (leftPos != 0 && rightPos + 1 != rhs.size)
      return SDFGeneratorError("Bracket not usable, since there no delimiter at start or end" + rhs, att).failNel[Unit]

    assert(leftPos < rightPos)

    val lower = rhs.patch(leftPos, List(AppArg), rightPos + 1)
    val higher = rhs.slice(leftPos + 1, rightPos)

    if (!syntaxStrings.contains(lower) && !defaultSyntax.contains(lower))
      return SDFGeneratorError("Could not find syntax for " + lower.mkString("", " ", ""), att).failNel[Unit]
    if (!syntaxStrings.contains(higher) && !defaultSyntax.contains(higher))
      return SDFGeneratorError("Could not find syntax for " + higher.mkString("", " ", ""), att).failNel[Unit]
    if (defaultSyntax.contains(lower) && defaultSyntax.contains(higher))
      return SDFGeneratorError("Can not redefine default priorities " + higher.mkString(" ") + " > " + lower.mkString(" "), att).failNel[Unit]

    //associativity case
    if (lower == higher) {
      if (leftPos == 0)
        syntaxStrings(higher).map { s => appendPriority(PLeftAssociativity(higher, s)) }
      else if (rightPos == rhs.size - 1)
        syntaxStrings(higher).map { s => appendPriority(PRightAssociativity(higher, s)) }
      else
        return SDFGeneratorError("Both syntax parts are the same, but associativity is ether left nor right: " + rhs, att).failNel[Unit]

    } //case for default syntax
    else if (defaultSyntax.contains(lower) || defaultSyntax.contains(higher)) {
      currentPriority = removeDefault(lower, higher)
      // term syntax
      if (termDefaultsMap.contains(lower)) {
        val ds = termDefaultsMap(lower)
        syntaxStrings(higher).map { s => appendPriority(Higher(higher, s, lower, ds, true)) }

      } else if (termDefaultsMap.contains(higher)) {
        val ds = termDefaultsMap(higher)
        syntaxStrings(lower).map { s => appendPriority(Higher(higher, ds, lower, s, true)) }
      }
      //typ syntax
      if (typeDefaultsMap.contains(lower)) {
        val ds = typeDefaultsMap(lower)
        syntaxStrings(higher).map { s => appendPriority(Higher(higher, s, lower, ds, true)) }

      } else if (typeDefaultsMap.contains(higher)) {
        val ds = typeDefaultsMap(higher)
        syntaxStrings(lower).map { s => appendPriority(Higher(higher, ds, lower, s, true)) }
      }
    } else //normal case
    {
      for {
        h <- syntaxStrings(higher)
        l <- syntaxStrings(lower)
      } appendPriority(Higher(higher, h, lower, l))
    }
    ().successNel[Error]
  }
}
