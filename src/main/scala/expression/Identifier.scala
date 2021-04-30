package expression
import context._
import value._

case class Identifier(val name: String) extends Expression {

  def execute(env: Environment):Value= {
    //env(this)
    var result : Value = env(this)
    if(result.isInstanceOf[Thunk]) {result = result.asInstanceOf[Thunk].apply()}
    result
  }
  override def hashCode = this.toString.hashCode()

  override def toString = name
}
