package expression
import context._
import value._

case class Lambda(val parameters: List[Identifier], val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    println("Lambda is called")
    val closure = new Closure(parameters, body, env)
    println("closure is created", closure)
    //println(closure.apply(parameters.map(_.execute(env))))
    println("DD")
    closure.apply(parameters.map(_.execute(env)))
  }
}
