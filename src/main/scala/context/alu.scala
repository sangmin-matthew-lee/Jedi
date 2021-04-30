package context

import value._
import expression._

object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = opcode.name match {
    case "add" => add(args)            // n-ary
    case "mul" => mul(args)            // n-ary
    case "sub" => sub(args)            // n-ary
    case "div" => div(args)            // n-ary
    case "less" => less(args)          // binary
    case "equals" => same(args)        // binary
    case "more" => more(args)          // binary
    case "unequals" => unequals(args)  // binary
    case "not" => not(args)            // unary
    case "write" => write(args)
    case "nil" => getEmpty()
    case "cons" => cons(args)
    case "car" => car(args)
    case "cdr" => cdr(args)
    case "list" => list(args)
//    // TBC
  }

  private def add(args: List[Value]): Value = {

    def helper(result: Addable, unseen: List[Value]): Addable =
      if(unseen == Nil) result
      else helper(result + unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: Addable => helper(args(0).asInstanceOf[Addable], args.tail )
      case _ => throw new TypeException("Inputs to + must be addable")
    }
  }

  private def mul(args: List[Value]): Value = {

    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen == Nil) result
      else helper(result * unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: value.Numeric => helper(args(0).asInstanceOf[value.Numeric], args.tail )
      case _ => throw new TypeException("Inputs to + must be Numeric")
    }
  }

  private def sub(args: List[Value]): Value = {

    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen == Nil) result
      else helper(result - unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: value.Numeric => helper(args(0).asInstanceOf[value.Numeric], args.tail )
      case _ => throw new TypeException("Inputs to + must be Numeric")
    }
  }

  private def div(args: List[Value]): Value = {

    def helper(result: value.Numeric, unseen: List[Value]): value.Numeric =
      if(unseen == Nil) result
      else helper(result / unseen.head, unseen.tail)

    if(args.size < 2) throw new TypeException("2 or more inputs required by +")
    args(0) match {
      case n: value.Numeric => helper(args(0).asInstanceOf[value.Numeric], args.tail )
      case _ => throw new TypeException("Inputs to / must be Numeric")
    }
  }

  private def less(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by <")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to < must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] < args(1))
  }

  private def more(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by >")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to > must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] > args(1))
  }

  private def same(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by ==")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to == must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] == args(1))
  }

  private def unequals(args: List[Value]): Value = {
    if(args.size != 2) throw new TypeException("2 inputs required by !=")
    if(!args(0).isInstanceOf[Ordered[Value]]) throw new TypeException("Inputs to != must be orderable")
    Boole(args(0).asInstanceOf[Ordered[Value]] != args(1))
  }

  private def not(args: List[Value]): Value = {
    if(!args(0).isInstanceOf[Value]) throw new TypeException("Inputs to ! must be valuable")
    args(0).asInstanceOf[Boole].unary_!()
  }

  private def write(args: List[Value]): Value = { println(args(0)); Notification.DONE }

  private def getEmpty():Value = empty

  private def cons(args:List[Value]) : Value = {
    if(args.size < 2) throw new TypeException("2 or more inputs required")
    //println(Pair(args(0),args(1)).isInstanceOf[Value] )
    Pair(args(0),args(1))
  }

  private def car(args:List[Value]) : Value = {
    args(0).asInstanceOf[Pair].first
  }

  private def cdr(args:List[Value]) : Value = {
    args(0).asInstanceOf[Pair].second
  }

  private def list(args:List[Value]) : Value = {
    if(args.size < 2) throw new TypeException("2 or more inputs required")
    val last = args.size - 1
    var lastPair = Pair(args(last),empty)
    var newPair = Pair(empty,empty)

    for(i <- (0 to last-1).reverse) {
      newPair = Pair(args(i),lastPair)
      lastPair = newPair
    }
    newPair
    //def p = list("a", "e" ,"i", "o", "u")
  }

}
