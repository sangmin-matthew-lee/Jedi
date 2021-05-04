package expression
import context._
import value._

case class Assignment(private var vbl:Identifier, private var update:Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    vbl.execute(env) match {
      case v: Variable => v.content = update.execute(env); Notification.DONE
      case _ => throw new IllegalValueException()
    }
  }
}

//def c = var(0)

//def z = lambda() c := [c]+1
