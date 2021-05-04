package expression
import context._
import value._

case class Iteration(private var condition: Expression, private var body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    ???
  }
}
