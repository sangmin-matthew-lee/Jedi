package value

case class Variable(val content: Value) extends Value {
  override def toString: String = "[" + content + "]"
}
