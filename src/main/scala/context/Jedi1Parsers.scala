package context

import scala.util.parsing.combinator._
import expression._
import value._

import java.nio.channels.NonReadableChannelException

/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def"~id~"="~exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if"~"("~cond~")"~cons~None => Conditional(cond, cons)
    case "if"~"("~cond~")"~cons~Some("else"~alt) => Conditional(cond, cons, alt)
  }

  def  disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil => con
    case con ~ more => Disjunction(con::more)
  }

  // conjunction ::= equality ~ ("&&" ~ equality)*
  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^ {
    case equ ~ Nil => equ
    case equ ~ more => Conjunction(equ::more)
  }

  // equality ::= inequality ~ ("==" ~ inequality)?
  def equality: Parser[Expression] = inequality ~ opt("==" ~ inequality) ^^ {
    case inequ ~ None => inequ
    case inequ ~ Some("==" ~ more) => FunCall(Identifier("equals"), inequ::Some(more).toList)
  }

  // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
  def inequality: Parser[Expression] = sum ~ opt((("<" | ">" | "!=") ~ sum)) ^^ {
    case sum ~ None => sum
    //case sum ~ more => FunCall(Identifier("unequals"), sum::more.toList)
    case sum ~ Some("<" ~ more) => FunCall(Identifier("less"),sum::Some(more).toList)
    case sum ~ Some(">" ~ more) => FunCall(Identifier("more"),sum::Some(more).toList)
    case sum ~ Some("!=" ~ more) => FunCall(Identifier("unequals"),sum::Some(more).toList)
  }

  // sum ::= product ~ ("+" | "-") ~ product)*
  def sum: Parser[Expression] = product ~ rep(("+"|"-") ~ product) ^^ {
    case p ~ more => parseSums(p, more)
  }

  // use tail recursion to imitate left reduce
  // parses a - b + c into add(sub(a, b), c)
  private def parseSums(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "+" ~ p => FunCall(Identifier("add"), List(exp, p))
        case "-" ~ p => FunCall(Identifier("sub"), List(exp, p))
      }
    if (unseen == Nil) result
    else parseSums(combiner(result, unseen.head), unseen.tail)
  }

  // product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*" | "/") ~ term )^^{
    case t ~ more => parseProduct(t,more)
  }

  private def parseProduct(result: Expression, unseen: List[String ~ Expression]): Expression = {
    def combiner(exp: Expression, next: String~Expression) =
      next match {
        case "*" ~ p => FunCall(Identifier("mul"), List(exp, p))
        case "/" ~ p => FunCall(Identifier("div"), List(exp, p))
      }
    if (unseen == Nil) result
    else parseProduct(combiner(result, unseen.head), unseen.tail)
  }

  def term: Parser[Expression]  = funCall | literal | "("~>expression<~")"

  def literal = boole | inexact | exact | chars | identifier

  // chars ::= any characters bracketed by quotes
  def chars: Parser[Chars] = """\"[^"]+\"""".r ^^ {
    case characters => Chars(characters.substring(1, characters.length - 1))
  }

  // exact ::= 0|(\+|-)?[1-9][0-9]*
  def exact: Parser[Exact] = """0|(\\+|-)?[1-9][0-9]*""".r ^^ {
    //case exact => Exact(exact.asInstanceOf[Int])
    case exact => Exact(exact.toInt)
  }

  // inexact ::= (\+|-)?[0-9]+\.[0-9]+
  def inexact: Parser[Inexact] = """(\+|-)?[0-9]+\.[0-9]+""".r ^^ {
    //case inexact => Inexact(inexact.asInstanceOf[Double])
    case inexact => Inexact(inexact.toDouble)
  }

  // boole ::= true|false
  def boole: Parser[Boole] = """true|false""".r ^^ {
    //case boole => Boole(boole.asInstanceOf[Boolean])
    case boole => Boole(boole.toBoolean)
  }

  // identifier ::= [a-zA-Z][a-zA-Z0-9]*
  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    case identifier => Identifier(identifier)
  }

  // funCall ::= identifier ~ operands
  def funCall: Parser[FunCall] = identifier ~ operands ^^ {
    case funCall ~ more => FunCall(funCall, more)
  }

  // operands ::= "(" ~ (expression ~ ("," ~ expression)*)? ~ ")"
  def operands: Parser[List[Expression]] = "(" ~ opt(expression ~ rep("," ~> expression)) ~ ")" ^^ {
    case "(" ~ None ~ ")" => List()
    case "(" ~ Some(op ~ more) ~ ")" => op::more
  }
}
