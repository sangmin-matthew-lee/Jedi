package expression
import context.Environment
import value.{Notification, Value}

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val id = Identifier(identifier.toString)


    println()
    Notification.OK
  }
}
