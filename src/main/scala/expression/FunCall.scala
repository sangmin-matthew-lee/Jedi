package expression
import context._
import value._

case class FunCall(val operator: Identifier, val operands:List[Expression]) extends Expression {
  //FunCall.execute begins by eagerly executing all of its operands.
  //This produces a list of values called arguments.
  override def execute(env: Environment): Value = {
      var arguments: List[Value] = Nil   //eager execution
      //val op = operator.execute(env)

     if (env.contains(operator)) {
       println("ID is new")
       if (flags.paramPassing == flags.BY_REF) {
         arguments = operands.map(new Thunk(_,env))   //args are Thunk now
       } else {
         arguments = operands.map(_.execute(env)) //args are not Thunk
       }
       //etc.
       operator.execute(env) match {
         case closure: Closure => closure.apply(arguments)
         case thunk: Thunk => thunk.apply()
       }
     }
     else {   // ALU
       println("ID is ALU")
       arguments = operands.map(_.execute(env))
       alu.execute(operator, arguments)
     }
    }
}

/*
          //operator is a closure created by a lambda
          if (env.contains(operator) && operator.execute(env).isInstanceOf[Closure]) {
            arguments = operands.map(_.execute(env))
            operator.execute(env) match {
              case closure: Closure => closure(arguments)
            }
          }
 */