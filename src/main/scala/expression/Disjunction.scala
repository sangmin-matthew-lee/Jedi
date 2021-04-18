package expression
import context.Environment
import value._

// ||
case class Disjunction(operands: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {

    def helper( unseen:List[Expression]):Boole={
      //When all expressions are false
      if(unseen.head == Nil) Boole(false)
      //if(unseen.head == Nil) throw new SyntaxException
      else if(unseen.head == true) Boole(true)
      else helper(unseen.tail)
    }
    helper(operands)
  }
}
