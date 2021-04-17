package expression
import context._
import value._

case class Identifier(val name: String) extends Expression {
  override def toString = name
  def execute(env: Environment):Value= {
    env(this)
  }
  override def hashCode = this.toString.hashCode()
}
