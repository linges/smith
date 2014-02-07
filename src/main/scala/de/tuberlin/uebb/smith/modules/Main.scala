package de.tuberlin.uebb.smith.modules
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.jsglr.client.Asfix2TreeBuilder
import org.spoofax.jsglr.client.Disambiguator
import org.spoofax.jsglr.client.InvalidParseTableException
import org.spoofax.jsglr.client.NullTreeBuilder
import org.spoofax.jsglr.client.ParseTable
import org.spoofax.jsglr.client.imploder.TermTreeFactory
import org.spoofax.jsglr.client.imploder.TreeBuilder
import org.spoofax.jsglr.io.FileTools
import org.spoofax.jsglr.io.SGLR
import org.spoofax.jsglr.shared.BadTokenException
import org.spoofax.jsglr.shared.SGLRException
import org.spoofax.jsglr.shared.Tools
import org.spoofax.terms.TermFactory
import org.spoofax.jsglr.client.imploder._
import org.spoofax.terms.io.binary.TermReader
import java.io.BufferedReader
import java.io.FileReader
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import org.spoofax.jsglr.client.imploder.ITreeFactory
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import org.spoofax.jsglr.client.imploder.IToken
import org.spoofax.interpreter.terms.IStrategoList;
import org.strategoxt.strc.construct_term_0_0
import de.tuberlin.uebb.smith.modules._
import org.kiama.output.PrettyPrinter
import java.io.File
import scalaz._
import Scalaz._
import TypeSynonyms._
import Syntax._
import Unification._
object Main extends AdaptiveParser with PrettyPrinter with Driver 
    with DependenceAnalysis with ContextChecker
    with TypeChecker with SDFGenerator{

  def main(args: Array[String]) {
    if(args.size < 2)
    {
      println("Usage: WORKING_DIR MODULE [TERM]")
      return
    }
    workingDir = args(0)
    val fileDir = new File(workingDir)
    val mainFile = args(1)
    val inputFile = buildPath(mainFile)
    val file = new File(inputFile)
    if(!fileDir.exists())
    {
      println("Did not find working directory: "+fileDir.getAbsolutePath())
      return
    }
    if(!file.exists())
    {
      println("Did not find input file: "+file.getAbsolutePath())
      return
    }
    var finalTerm = "main"
    if(args.size == 3)
        finalTerm = args(2)

    val input = FileTools.loadFileAsString(new BufferedReader(new FileReader(inputFile)));
    run(mainFile, finalTerm)
  }
  
}
