package expression
import context.{BreakException, Environment}
import value.{Notification, Value}

case class Break() extends SpecialForm {
  override def execute(env: Environment): Value = {
    throw new BreakException()
    Notification.DONE
  }
}
