package de.tuberlin.uebb.smith.modules

import org.kiama.output.PrettyPrinter
import scalaz._
import Scalaz._
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import scala.collection.mutable.ListBuffer
import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException
import scala.collection.JavaConverters._
import TypeSynonyms._

trait DependenceAnalysis {
  self: Driver =>

  type Dag = DirectedAcyclicGraph[Identifier, DefaultEdge]

  def getTopologicalOrder(startModule: Identifier): ValidationNEL[Error, List[Identifier]] =
    {
      dependencyGraph = new DirectedAcyclicGraph[Identifier, DefaultEdge](classOf[DefaultEdge])
      for (
        unit <- buildDependencyGraph(startModule, true)
      ) yield {
        dependencyGraph.iterator().asScala.toList
      }
    }

  private var dependencyGraph = new DirectedAcyclicGraph[Identifier, DefaultEdge](classOf[DefaultEdge])

  private def buildDependencyGraph(moduleName: Identifier, init: Boolean): ValidationNEL[Error, Unit] =
    {
      if (init) {
        //adding dummy module for command line expression
        //which imports the given module
        dependencyGraph.addVertex(ID("cmd"))
        dependencyGraph.addVertex(moduleName)
        dependencyGraph.addDagEdge(moduleName, ID("cmd"))
      }
      try {
        for (
          m <- loadModuleHeader(moduleName);
          l <- m.imports.map({ mo =>
            dependencyGraph.addVertex(mo.importedModule.name)
            dependencyGraph.addDagEdge(mo.importedModule.name, m.name)
            buildDependencyGraph(mo.importedModule.name, false)
          }).sequence[({ type l[a] = ValidationNEL[Error, a] })#l, Unit]
        ) yield ()
      } catch { case e: CycleFoundException => 
          ModuleError("Cycle Found in " + moduleName + " ", EmptyAttribute).failNel[Unit]
                case e: IllegalArgumentException => 
          InternalError(moduleName + " " + e.getMessage, EmptyAttribute).failNel[Unit] }
    }

}
