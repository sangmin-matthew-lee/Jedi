package expression
import context._
import value._

case class FunCall(operator: Identifier, operand:List[Value]) extends Expression {
  //FunCall.execute begins by eagerly executing all of its operands.
  //This produces a list of values called arguments.
  override def execute(env: Environment): Value = {
    alu.execute(operator, operand)
  }
}
