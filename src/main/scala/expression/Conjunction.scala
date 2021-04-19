package expression
import context.{Environment, SyntaxException}
import value._

import scala.util.control.Breaks.break

// &&
 case class Conjunction(val operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {

    var result = Boole.TRUE
    for (exp <- operands if result == Boole.TRUE) {
      if (exp.execute(env) == Boole.FALSE) {
        result = Boole.FALSE
      }
    }
    result
  }
}