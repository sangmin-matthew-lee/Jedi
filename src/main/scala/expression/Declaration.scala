package expression
import context.Environment
import value.{Notification, Value}

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm {
  // bind identifiers to the values of expressions:
  override def execute(env: Environment): Value = {
    //Identifier(expression.execute(env).toString)
    Identifier(expression.toString)
    println()
    Notification.OK
  }
}
