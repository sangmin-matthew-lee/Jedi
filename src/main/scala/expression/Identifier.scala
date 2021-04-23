package expression
import context._
import value._

case class Identifier(val name: String) extends Expression {
  override def toString = name
  def execute(env: Environment):Value= {
    env(this)
    /*
    Value result = env(this)
    if (result.isInstanceOf[Thunk] result = result.asInstanceOf[Thunk].apply(Nil)
    result
     */
  }
  override def hashCode = this.toString.hashCode()
}
