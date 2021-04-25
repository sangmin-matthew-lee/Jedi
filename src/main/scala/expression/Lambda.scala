package expression
import context._
import value._

case class Lambda(val parameters: List[Identifier], val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    println("Lambda is called")
    val closure = new Closure(parameters, body, env)
    println(closure)
    //println(closure(parameters.map(_.execute(env))))
    println("OK")
    closure(parameters.map(_.execute(env)))
  }
}
