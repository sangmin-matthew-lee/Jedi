package expression
import context._
import value._

case class Dereference(private var content:Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    println(content.execute(env).isInstanceOf[String])
    println(content.execute(env).isInstanceOf[Value])
    println(content.execute(env).isInstanceOf[Variable])
    content.execute(env)
  }

  override def toString: String = content.toString
}
