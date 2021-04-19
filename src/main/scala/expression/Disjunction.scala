package expression
import context.Environment
import value._

// ||
case class Disjunction(operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {

    var result = Boole.FALSE
    for(exp <- operands if result==Boole.FALSE){
      if(exp.execute(env)==Boole.TRUE){
        result = Boole.TRUE
      }
    }
    result
  }
}