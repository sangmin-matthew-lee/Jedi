package expression
import context._
import value._

case class Lambda(val parameters: List[Identifier], val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val closure = new Closure(parameters, body, env)
    closure.apply(parameters.map(_.execute(env)))
  }
}
