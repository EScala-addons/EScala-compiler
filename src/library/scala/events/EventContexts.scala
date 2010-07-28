package scala.events

import scala.collection.mutable.{WeakHashMap,ListBuffer}

class EventContext {

  trait Enter[T] extends EventNode[T] {
    abstract override def reactions(id: Int, v: T, reacts: ListBuffer[() => Unit]) {
      depth += 1
      super.reactions(id, v, reacts)
    }
  }
  trait Leave[T] extends EventNode[T] {
    abstract override def reactions(id: Int, v: T, reacts: ListBuffer[() => Unit]) {
      depth -= 1
      super.reactions(id, v, reacts)
    }
  }

  // TODO make it thread local
  private var depth = 0

  def within = depth > 0

}

object EventContext {

  private val contexts = WeakHashMap.empty[Any, EventContext]

  def apply(key: Any) = contexts.getOrElseUpdate(key, new EventContext)
}

// vim: set ts=2 sw=2 et:
