package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~ opt(identifier ~ rep("," ~> identifier)) ~ ")" ^^ {
    case "(" ~ None ~ ")" => List()
    case "(" ~ Some(id ~ more) ~ ")" => id::more
  }

  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ param ~ exp => Lambda(param, exp)    //???
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] =  "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
    case "{" ~ block ~ Nil ~ "}" => Block(block::Nil)
    case "{" ~ block ~ more ~ "}" =>  Block(block::more)
  }

  // override of term parser
  override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"
}
