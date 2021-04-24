package expression
import context._
import value._

case class FunCall(val operator: Identifier, val operands:List[Expression]=null) extends Expression {
  //FunCall.execute begins by eagerly executing all of its operands.
  //This produces a list of values called arguments.
  override def execute(env: Environment): Value = {
      val arguments: List[Value] = operands.map(_.execute(env))   //eager execution
        //operator is a closure created by a lambda
        if(env.contains(operator) && operator.execute(env).isInstanceOf[Closure]) {
          print("This is closure!")
          ???
        }
        else alu.execute(operator, arguments)       // operator is from alu
    }

}
