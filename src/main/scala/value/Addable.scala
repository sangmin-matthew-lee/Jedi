package value

import expression._

//trait Addable extends Value with etc.
trait Addable extends Value with Literal{
  def +(other:Value): Addable
}
