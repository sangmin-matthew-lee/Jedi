package expression
import context._
import value._

case class FunCall(operator: Identifier, operands:List[Expression]) extends Expression {
  //FunCall.execute begins by eagerly executing all of its operands.
  //This produces a list of values called arguments.
  override def execute(env: Environment): Value = {
    val arg: List[Value] = operands.map(_.execute(env))
    try{
      alu.execute(operator, arg)
    }catch {
      case e: UndefinedException => throw e
    }
  }
}
