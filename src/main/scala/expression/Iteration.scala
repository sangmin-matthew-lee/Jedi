package expression
import context._
import value._

case class Iteration(private var condition: Expression, private var body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {

    var c1 = condition.execute(env)
    if(!c1.isInstanceOf[Boole]) throw new TypeException("Type error!")
    var c2 = c1.asInstanceOf[Boole]
    var break = false
    while (c2 == Boole.TRUE && !break){
      try{
        body.execute(env)
        c1 = condition.execute(env)
        if(!c1.isInstanceOf[Boole]) throw new TypeException("Type Error!")
        c2 = c1.asInstanceOf[Boole]
      }catch {
        case e: BreakException => break = true
      }
    }
    Notification.DONE
//    while(condition.execute(env) == Boole.TRUE) {
//    body.execute(env)
//    }
//    Notification.DONE
  }

}

//def c = var(0)

//while ([c] < 10) { write("calling incCount");c := [c] + 1 }