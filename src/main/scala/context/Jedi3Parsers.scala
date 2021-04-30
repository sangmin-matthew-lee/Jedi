package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ ":=" ~ expression
  def assignment: Parser[Assignment] = identifier ~ ":=" ~ expression ^^ {
    case id ~ ":=" ~ exp =>  Assignment(id,exp)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
    case "while" ~ "(" ~ condition ~ ")" ~ body => Iteration(condition,body)
  }
  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Dereference] = "[" ~ expression ~ "]" ^^ {
    case "[" ~ exp ~ "]" => Dereference(exp)
  }
  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | freeze  | funCall | block |  assignment | dereference | literal | "("~>expression<~")"

}