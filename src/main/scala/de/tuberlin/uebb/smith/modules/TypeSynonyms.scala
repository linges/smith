package de.tuberlin.uebb.smith.modules

import scalaz._
import Scalaz._
import de.tuberlin.uebb.smith.modules._

object TypeSynonyms
{

  type IdentifierMapping = Map[Identifier, QID]
  type LocalTypeContext = Map[Identifier, Type]
  type Rest = String
  type Result[A] = ValidationNEL[Error, A]


}
