package expression
import context._
import value._

case class FunCall(val operator: Identifier, val operands:List[Expression]=null) extends Expression {
  //FunCall.execute begins by eagerly executing all of its operands.
  //This produces a list of values called arguments.
  override def execute(env: Environment): Value = {
    if(operands != null){       //if operands are not list of zero
      val arguments: List[Value] = operands.map(_.execute(env))
      try{
        alu.execute(operator, arguments)
      }catch {
        case e: UndefinedException => throw e
      }
    }else {   //if operand is list of zero -- error?
      throw new JediException()
    }

  }
}
