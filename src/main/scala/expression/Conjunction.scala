package expression
import context.Environment
import value._

class Conjunction(operands: List[Value]) extends SpecialForm {
  override def execute(env: Environment): Value = {
???
//    def helper(result:Boolean, unseen:List[Value]):Boolean = {
//      if(unseen.head == false) result
//      else helper(result,unseen.tail)
//    }

  }
}
