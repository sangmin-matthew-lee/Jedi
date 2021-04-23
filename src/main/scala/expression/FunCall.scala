package expression
import context._
import value._

case class FunCall(val operator: Identifier, val operands:List[Expression]=null) extends Expression {
  //FunCall.execute begins by eagerly executing all of its operands.
  //This produces a list of values called arguments.
  override def execute(env: Environment): Value = {
      val arguments: List[Value] = operands.map(_.execute(env))   //eager execution
      try{
        //operator is a closure created by a lambda
        if(operator.isInstanceOf[Closure]) {
          operator.asInstanceOf[Closure]

        }
        else alu.execute(operator, arguments)       // operator is from alu
      }catch {
        case e: UndefinedException => throw e
      }
    }

}
