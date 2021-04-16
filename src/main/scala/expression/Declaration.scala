package expression
import context.Environment
import value.Value

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = ???
    val id = Identifier(identifier.toString)

}
