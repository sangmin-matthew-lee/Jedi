package expression
import context.Environment
import value.{Notification, Value}

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression=null) extends SpecialForm {
  override def execute(env: Environment): Value = {

    if(condition == true) {
      consequent.execute(env)
    } else {  //condition is false -> execute alternative
      if(alternative == null) {
        Notification.UNSEPCIFIED
      }else {
        alternative.execute(env)
      }
    }
  }
}
