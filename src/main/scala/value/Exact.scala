package value
import context._

case class Exact(val value: Int) extends Numeric with Ordered[Value] {

  def +(other: Value): Addable =
    other match {
      case x: Exact => Exact(this.value + x.value)
      case x: Inexact => Inexact(this.value.toDouble + x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def unary_-(): Numeric = Exact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Exact => this.value.compare(x.value)
      case x: Inexact => this.value.toDouble.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  override def equals(other: Any): Boolean =
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value.toDouble
      case x: Exact => x.isInstanceOf[Exact] && x.value == this.value
      case _ => false
    }

  override def toString = this.value.toString

  override def hashCode(): Int = this.value.toString.hashCode()

  // *, -, /, hashCode, etc.
  def -(other: Value): Numeric =
    other match {
      case x: Exact => Exact(this.value - x.value)
      case x: Inexact => Inexact(this.value.toDouble - x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def *(other: Value): Numeric =
    other match {
      case x: Exact => Exact(this.value * x.value)
      case x: Inexact => Inexact(this.value.toDouble * x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def /(other: Value): Numeric = {
    if(other == Exact(0)) throw new IllegalValueException("Divided by 0!")

    other match {
      case x: Exact => Exact(this.value / x.value)
      case x: Inexact => Inexact(this.value.toDouble / x.value)
      case _ => throw new TypeException("Numeric operand required")
    }
  }

}