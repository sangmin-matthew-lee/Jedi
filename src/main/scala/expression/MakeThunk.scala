package expression
import context.Environment
import value.Value

case class MakeThunk(body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    println("Make Thunk: " + body.execute(env))
    body.execute(env)
  }
}
