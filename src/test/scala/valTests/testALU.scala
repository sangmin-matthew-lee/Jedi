package valTests
import context._
import value._
import expression._

object testALU extends App {
  try {
    println(alu.execute(Identifier("add"), List(Exact(5), Exact(6), Exact(7))))     // 18
    println(alu.execute(Identifier("add"), List(Chars("abc"), Exact(6), Exact(7)))) // abc67
    println(alu.execute(Identifier("less"), List(Chars("abc"), Chars("def"))))  // true
    println(alu.execute(Identifier("equals"), List(Chars("abc"), Chars("abc"))))  // true
    println("\nCustom tests")
    println(alu.execute(Identifier("mul"), List(Exact(5), Exact(6))))     // 30
    println(alu.execute(Identifier("mul"), List(Exact(5), Exact(-6))))     // -30
    println(alu.execute(Identifier("sub"), List(Exact(5), Exact(6))))     // -1
    println(alu.execute(Identifier("sub"), List(Exact(6), Exact(5))))     // 1
    println(alu.execute(Identifier("sub"), List(Inexact(6), Inexact(5))))     // 1.0
    println(alu.execute(Identifier("div"), List(Exact(12), Inexact(6))))     // 2.0
    println(alu.execute(Identifier("div"), List(Exact(-12), Exact(12))))     // -1
    println(alu.execute(Identifier("more"), List(Chars("abc"), Chars("def"))))  // false
    println(alu.execute(Identifier("not"), List(Boole(false))))  // true
    println(alu.execute(Identifier("not"), List(Boole(true))))  // false

  } catch {
    case e: Exception => println(e)
  }


}