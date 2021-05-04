package expression
import context._
import value._

case class Iteration(private var condition: Expression, private var body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
//    println("codition: " + condition)
//    println("codition.exe(env): " + condition.execute(env)) //true or false
//    println("body: " + body)
//    println("body.exe(env): " + body.execute(env))
    while(condition.execute(env) == Boole.TRUE) {
      //run body
      body.execute(env)
    }
    Notification.DONE
  }
}

//def c = var(0)

//while ([c] < 10) { write("calling incCount");c := [c] + 1 }