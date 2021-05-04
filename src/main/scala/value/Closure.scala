package value

import context._
import expression._

class Closure(val parameters:List[Identifier], val body: Expression, val defEnv:Environment) extends Value {

    def apply(args:List[Value]): Value = {
      val tempEnv = new Environment(defEnv)   //create temp Env extending def Env
      tempEnv.bulkPut(parameters, args)     //bind paramters to arguments in temp Env
      body.execute(tempEnv)
    }
}

/*
		1. create tempEnv extending defEnv(DE)
		2. bind params to args in tempEnv
		3. execute body in tempEnv
 */