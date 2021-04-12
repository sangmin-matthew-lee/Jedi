package value

import context._

class Notification(val msg:String) extends Value {
  override def toString: String = msg

//  val OK = new Notification(msg)
//  val DONE = new Notification(msg)
//  val UNSEPCIFIED = new Notification(msg)
}

object Notification {
  def apply(msg: String) = {
    new Notification(msg)
//    val OK = new Notification(msg)
//    val DONE = new Notification(msg)
//    val UNSEPCIFIED = new Notification(msg)
  }
}
