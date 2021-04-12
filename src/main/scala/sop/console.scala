package sop

import scala.io._

object console {

  val parsers = new SOPParsers

  def execute(cmmd: String): String = {
    val result = parsers.parseAll(parsers.expression, cmmd)
    result match {
      case result: parsers.Failure => throw new Exception("syntax error")
      case _ => {
        val tree = result.get  // get the expression from the tree
        tree.toString
      }
    }
  }

  def repl() {
    var more = true
    while(more) {
      try {
        print("-> ")
        val cmmd = StdIn.readLine
        if (cmmd == "quit") more = false
        else println(execute(cmmd))
      } catch {
        case e: Exception => println(e)
      }
    }
    println("bye")
  }

  def main(args: Array[String]): Unit = { repl() }

}