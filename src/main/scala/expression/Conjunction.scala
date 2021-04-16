package expression
import context.{Environment, SyntaxException}
import value._

// &&
 case class Conjunction(operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {

    def helper(unseen:List[Expression]):Boole = {

      //When all expressions are true
      if(unseen.head == Nil) Boole(true)
      //if(unseen.head == Nil) throw new SyntaxException
      else if(unseen.head == false) Boole(false)
      else helper(unseen.tail)
    }
    helper(operands)
  }
}
