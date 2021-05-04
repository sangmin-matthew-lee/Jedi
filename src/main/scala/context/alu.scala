package context

import value._
import expression._

/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {

  def execute(opcode: Identifier, args: List[Value]): Value = opcode.name match {
    case "add" => add(args)
    case "mul" => mul(args)
    case "sub" => sub(args)
    case "div" => div(args)
    case "less" => less(args)
    case "more" => more(args)
    case "equals" => equals(args)     //it was same in Jedi1.0
    case "unequals" => unequals(args)
    case "not" => not(args)
    //Pair
    case "nil" => getEmpty()
    case "cons" => cons(args)
    case "car" => car(args)
    case "cdr" => cdr(args)
    case "list" => list(args)
    // variables
    case "dereference" => dereference(args)
    case "var" => makeVar(args)
    // primitive I/O ops:
    case "write" => write(args)
    // case "prompt" => prompt(args)
    // case "read" => read(args)

    /*
    // store ops
    case "store" => store(args)
    case "put" => put(args)
    case "rem" => rem(args)
    case "contains" => contains(args)
    case "map" => map(args)
    case "filter" => filter(args)
    case "get" => get(args)
    case "addLast" => addLast(args)
    case "size" => size(args)
    */

    case _ => throw new UndefinedException(opcode)
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

  private def equals(args: List[Value]): Value = {
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

  // creates a new variable containing args(0)
  private def makeVar(args:List[Value]): Value = {
    Variable(args(0))
  }

  // returns the content of args(0)
  private def dereference(args:List[Value]):Value = {
    if(args(0).isInstanceOf[Variable]){
      args(0).asInstanceOf[Variable].content
    }
    else {
      throw new TypeException()
    }
  }

/*
 // store ops
 // returns a new store containing args
 private def store(args: List[Value]) = {Store(args)}

 // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
 private def put(args: List[Value]) = {
   if (args.size != 3)
      throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
   if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store])
      throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
   args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
   Notification.DONE
 }

 // rem(p: Integer, s: Store) calls s.rem(p)
 private def rem(args: List[Value]) = {???}

 // get(p: Integer, s: Store) calls s.get(p)
 private def get(args: List[Value]) = {???}

 // map(f: Closure, s: Store) calls s.map(f)
 private def map(args: List[Value]) = {???}

 // filter(f: Closure, s: Store) calls s.filter(f)
 private def filter(args: List[Value]) = {???}

 // contains(v: Value, s: Store) calls s.contains(v)
 private def contains(args: List[Value]) = {???}

 // addLast(v: Value, s: Store) calls s.add(v)
 private def addLast(args: List[Value]) = {???}

 // size(s: Store) calls s.size
 private def size(args: List[Value]) = {???}

 */

}
