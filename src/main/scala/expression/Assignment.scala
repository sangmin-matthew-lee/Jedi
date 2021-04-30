package expression
import context._
import value._

case class Assignment(private var vbl:Identifier, private var update:Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    println(update.execute(env))
    update.execute(env)
  }
}
