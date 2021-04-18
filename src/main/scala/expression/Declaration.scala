package expression
import context.Environment
import value.{Notification, Value}

case class Declaration(var identifier: Identifier, var expression: Expression) extends SpecialForm {
  // bind identifiers to the values of expressions:
  override def execute(env: Environment): Value = {
    env += identifier -> expression.execute(env)
    Notification.OK
  }
}
