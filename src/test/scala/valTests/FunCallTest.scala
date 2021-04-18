package valTests

import expression._
import context._
import value._


object FunCallTest extends App {
  val globalEnvironment = new Environment
  //val operands = List(Integer(6), Integer(7))
  val operands = List(Exact(6), Exact(7))
  var exp = FunCall(Identifier("add"), operands)
  println(exp.execute(globalEnvironment))   //13
  exp = FunCall(Identifier("less"), operands)
  println(exp.execute(globalEnvironment))   //6<7 true
  exp = FunCall(Identifier("mul"), operands)
  println(exp.execute(globalEnvironment))   //42
  exp = FunCall(Identifier("unequals"), operands)
  println(exp.execute(globalEnvironment))   //true
}