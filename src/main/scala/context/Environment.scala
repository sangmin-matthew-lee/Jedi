package context

import expression.Identifier
import value.Value

import scala.collection.mutable

class Environment extends collection.mutable.HashMap [Identifier, Value]
