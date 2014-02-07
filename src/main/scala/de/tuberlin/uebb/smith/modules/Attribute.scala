package de.tuberlin.uebb.smith.modules


sealed abstract class Attribute 
{
  override def equals(that: Any) : Boolean = that match
    { 
      case x : Attribute => true
      case _ => false
    }
  def merge(that: Attribute): Attribute
}
case class FileAttribute(file: String) extends Attribute
{
  override def toString() : String =  file
  def merge(that: Attribute): Attribute = that
}

case class PosAttribute(start: Position, end : Position, file : String) extends Attribute
{
  override def toString() : String = start + "-" + end + " in " + file
  def merge(that: Attribute): Attribute =
     that match {
      case PosAttribute(_, end, _) =>
        PosAttribute(start, end, file)
      case x  => this 
    }
}

case class Position(line: Int, column: Int) 
{
  override def equals(that: Any) : Boolean = that match
    { 
      case x : Position => true
      case _ => false
    }

  //jsglr starts counting with 0, so do we internally.
  //but my emacs starts counting lines with 1 and columns with 0
  //so thats what I print
  override def toString() : String = (line+1) +":"+ column
}

object EmptyAttribute extends Attribute
{
  override def toString() : String = ""
  def merge(that: Attribute): Attribute = that
}

