package value

import expression._

case class Boole(value : Boolean) extends Literal {

  def &&(other: Value): Boole = {
    other match {
      case x: Boole => Boole(true)
      case _=> Boole(false)
    }
  }

  def ||(other: Value): Boole = {
    other match {
      case x: Boole => Boole(true)
      case _=> Boole(false)
    }
  }

  def unary_!():Boole = Boole(!this.value)

  override def equals(other: Any): Boolean = {
    other match {
      case x: Boole => x.isInstanceOf[Boole] && x.value == this.value
      case _=> false
    }
  }

  override def toString: String = this.value.toString

  override def hashCode(): Int = this.value.toString.hashCode()

}

object Boole{
  def TRUE: Boole = {
    Boole(true)
  }

  def FALSE: Boole = {
    Boole(false)
  }
}