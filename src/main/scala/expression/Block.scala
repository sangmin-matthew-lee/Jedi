package expression
import context.Environment
import value.Value

case class Block(val exps:List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)    //Temp Env extends GE
    var result = new Value {}

    for (exp <- exps) {
      result = exp.execute(tempEnv)
    }
    result
  }
}

/*
-> {write(1); write(2); write(3); 1 + 2 + 3}
1
2
3
6
-> {def x = 1; def y = 2; def z = 3; x + y + z}
6
-> x
Undefined identifier: x
 */