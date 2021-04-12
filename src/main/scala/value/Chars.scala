package value

import context._

//class Chars extends Addable with etc.
case class Chars(val value: String) extends Addable with Ordered[Value] {

  def +(other: Value): Addable = {
    other match {
      case x: Exact => Chars(this.value.concat(x.value.toString))
      case x: Inexact => Chars(this.value.concat(x.value.toString))
      case x: Chars => Chars(this.value.concat(x.value))
      case _=> throw new TypeException("Addable operand required")
    }
  }

  override def compare(other: Value): Int = {
    other match {
      case x: Chars => this.value.compare(x.value)
      case _=> throw new TypeException("Arguments must be comparable")
    }

  }

  override def equals(other: Any): Boolean = {
    other match {
      case x: Chars => x.isInstanceOf[Chars] && x.value == this.value
      case _=> false
    }
  }

  // It is already String
  override def toString: String = this.value

  override def hashCode(): Int = this.value.hashCode()

  def size(): Exact = {
    Exact(this.value.length)
  }

  def subChars(to:Exact, from:Exact): Chars = {
    Chars(this.value.substring(to.value,from.value))
  }
}
