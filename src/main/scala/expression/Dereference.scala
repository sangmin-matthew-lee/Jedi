package expression
import context._
import value._

case class Dereference(private var body:Expression) extends SpecialForm {
  override def execute(env: Environment): Value = ???
}
