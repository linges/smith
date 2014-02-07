package de.tuberlin.uebb.smith.modules
import org.kiama.output.PrettyPrinter
import scala.collection.mutable.ListBuffer
import Syntax._
import TypeSynonyms._
import scalaz._
import Scalaz._

trait ContextChecker {
  self: Driver =>

  /**
    * Builds dictionary for a given module header
    */
  def buildRenaming(mh: ModuleHeader, modules: Map[Identifier, Module]): ValidationNEL[Error, Dictionary] = {
    val (publicImports, privateImpotrs) = mh.imports.partition { x: Import => x.visibility == Public }
    val att = FileAttribute(mh.fileName)
    val pubR = mergeMaps(publicImports.map(buildRenamingFromImport(_, modules)), att)
    val priR = mergeMaps(privateImpotrs.map(buildRenamingFromImport(_, modules)), att)
    for {
      pub <- pubR
      pri <- priR
      _ <- combine(pub, pri, att) // checking for collsion between public and private renamings
    } yield Dictionary(pri, pub)
  }

  //first without, then only, then renaming and then qualification
  //that means modifications use nonqualified identifiers
  def buildRenamingFromImport(imp: Import, modules: Map[Identifier, Module]): ValidationNEL[Error, IdentifierMapping] = {
    val n = imp.importedModule.name

    if (!modules.contains(n)) return ContextError("Could not find Module " + n, imp.att).failNel[IdentifierMapping]
    val mod = modules(n)
    var names: IdentifierMapping = mod.dic.publicMap
    val (re, only, without) = cleanUpImportMods(imp.importModifications)

    //WITHOUT 
    val woErr = 
      without.map { wo =>
       if (!names.contains(wo)) 
         ContextError("Without: "+ wo + " is not part of the exported names from " + imp.importedModule.name, imp.att).failNel[Unit]
       else ().successNel[Error]
      }
    without.foreach(x => names = names - x)

    //ONLY
    val onErr = 
      only.map { on =>
       if (!names.contains(on)) 
         ContextError("Only: " +on + " is not part of the exported names from " + imp.importedModule.name, imp.att).failNel[Unit]
       else ().successNel[Error]
      }
    if (!only.isEmpty)
      names = names.filterKeys { k => only.contains(k) }


    //RENAMING
    val reErr = 
      re.map { er =>
       if (!names.contains(er.from)) 
         ContextError("Renaming: " +er.from + " is not part of the exported names from " + imp.importedModule.name, imp.att).failNel[Unit]
       else ().successNel[Error]
      }
    val errs = (woErr ++ onErr ++ reErr).sequence[({ type l[a] = Result[a] })#l, Unit] 
    errs match {
      case x: Success[_,_] => ()
      case Failure(e) => return e.fail[IdentifierMapping]
    }

    re.foreach(ir =>
      names = names.map {
        case (k, v) =>
          if (k == ir.from && names.contains(ir.to))
            return ContextError("Renaming not possible " + ir.to + " already exist in " + imp.importedModule.name, imp.att ).failNel[IdentifierMapping]
          else if (k == ir.from)
            (ir.to, v)
          else (k, v)
      })
    val ren: IdentifierMapping = imp.importedModule match {
      case NotQualified(n) => names
      case Qualified(n) => names.map { case (k, v) => (n ~ k, v): (Identifier, QID) }
      case QualifiedAs(n, as) => names.map { case (k, v) => (as ~ k, v): (Identifier, QID) }
    }
    ren.successNel[Error]
  }


  def cleanUpImportMods(l: List[ImportModification]) =
    {
      val re: ListBuffer[IdentifierRenaming] = new ListBuffer()
      val only: ListBuffer[Identifier] = new ListBuffer()
      val without: ListBuffer[Identifier] = new ListBuffer()

      l.foreach {
        case ir: ImportRenaming => re ++= ir.ren
        case wo: Without => without ++= wo.wos
        case o: Only => o.onlys.foreach {
          case i: Identifier => only += i
          case ir: IdentifierRenaming => re += ir; only += ir.from
        }

      }
      (re.toList, only.toList, without.toList)
    }


  def getPublicImports(m: Module): List[Import] = m.imports.filter(x => x.visibility == Public)
  def getPublicDefs(m: Module): List[Def] = {
    val a: ListBuffer[Def] = new ListBuffer()
    m.valDef.filter(_._2.isPublic).foreach     (x => a += x._2)
    m.typeDef.filter(_._2.isPublic).foreach    (x => a += x._2)
    m.binderDef.filter(_._2.isPublic).foreach  (x => a += x._2)
    m.bindingDef.filter(_._2.isPublic).foreach (x => a += x._2)
    m.bracketDef.filter(_._2.isPublic).foreach (x => a += x._2)
    a.toList
  }

  def mergeMaps(l: List[ValidationNEL[Error, IdentifierMapping]], att: Attribute): ValidationNEL[Error, IdentifierMapping] =
    l.foldLeft(Map[Identifier, QID]().successNel[Error])(
      {
        case (z, x) =>
          for (
            zz <- z;
            xx <- x;
            r <- combine(zz, xx, att)
          ) yield r
      })

  def combine[A, B](m1: Map[A, B], m2: Map[A, B], att: Attribute): ValidationNEL[Error, Map[A, B]] = {
    val k1 = m1.keySet 
    val k2 = m2.keySet 
    val kk = k1 intersect k2
    if (kk.isEmpty)
      (m1 ++ m2).successNel[Error]
    else
    {
      val l1 = m1.filterKeys(kk.contains(_)).mkString("\n")
      val l2 = m2.filterKeys(kk.contains(_)).mkString("\n")
      ContextError("Importing duplicated Identifier: \n" + l1 + "\n"+"and\n" + l2, att).failNel[Map[A, B]]
    }
  }
}
