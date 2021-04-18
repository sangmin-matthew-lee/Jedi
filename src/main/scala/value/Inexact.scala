package value

import context._

case class Inexact(val value: Double) extends Numeric with Ordered[Value] {

  def +(other: Value): Addable =
    other match {
      case x: Exact => Inexact(this.value + x.value)
      case x: Inexact => Inexact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def unary_-(): Numeric = Inexact(-this.value)

  override def compare(other: Value): Int =
    other match {
      case x: Exact => this.value.toInt.compare(x.value)
      case x: Inexact => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  override def equals(other: Any): Boolean =
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value
      case x: Exact => x.isInstanceOf[Exact] && x.value == this.value.toInt
      case _ => false
    }

  override def hashCode(): Int = this.value.toString.hashCode()

  override def toString = this.value.toString

  // *, -, /, hashCode, etc.
  def -(other: Value): Numeric =
    other match {
      case x: Exact => Exact(this.value.toInt - x.value)
      case x: Inexact => Inexact(this.value - x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def *(other: Value): Numeric =
    other match {
      case x: Exact => Exact(this.value.toInt * x.value)
      case x: Inexact => Inexact(this.value * x.value)
      case _ => throw new TypeException("Numeric operand required")
    }

  def /(other: Value): Numeric = {
    if(other == Inexact(0.0)) throw new IllegalValueException("Divided by 0!")

    other match {
      case x: Exact => Exact(this.value.toInt / x.value)
      case x: Inexact => Inexact(this.value / x.value)
      //case x if other == Inexact(0) => throw new IllegalValueException("Divided by 0!")
      case _ => throw new TypeException("Numeric operand required")
    }
  }
}