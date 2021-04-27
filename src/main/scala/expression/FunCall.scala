package expression
import context._
import value._

case class FunCall(val operator: Identifier, val operands:List[Expression]) extends Expression {
  //FunCall.execute begins by eagerly executing all of its operands.
  //This produces a list of values called arguments.
  override def execute(env: Environment): Value = {
      var arguments: List[Value] = Nil   //eager execution
      //val op = operator.execute(env)

          //operator is a closure created by a lambda
          if (env.contains(operator) && operator.execute(env).isInstanceOf[Closure]) {
            arguments = operands.map(_.execute(env))
            operator.execute(env) match {
              case closure: Closure => closure(arguments)
            }
          }
          else {
            arguments = operands.map(_.execute(env))
            alu.execute(operator, arguments)
          } // operator is from alu

    }
}

//def fun1 = lambda(x, y) 3 * x + 2 * y
//ok
//-> fun1(5, 7)
//29