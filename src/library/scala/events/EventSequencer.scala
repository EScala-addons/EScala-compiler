package scala.events

import scala.actors._
import scala.collection.mutable.{HashMap, ListBuffer}

trait EventSequencer {
  /**  The events are cached by the sequencer to allow to unregister reactions and sinks. */
  object async extends Actor {

    // TODO improve typing with dependent type
    // see http://scala-programming-language.1934581.n4.nabble.com/scala-Real-dependent-typing-in-Scala-td1998734.html
    object cache extends HashMap[Event[_], SequencedEvent[_]]

    def apply[T](ev: Event[T]): Event[T] = synchronized {
      cache.getOrElseUpdate(ev, new SequencedEvent(ev)).asInstanceOf[SequencedEvent[T]]
    }

    def act {
      loop {
        react {
          case OnEvent(ev, id, v, reacts) =>
            ev.reactions(id, v, reacts)
        }
      }
    }

    // start the actor
    start
  }

  /** This event is triggered whenever the source event is triggered.
   *  It simply delegates the reaction collection to an actor in order
   *  to make it asynchronous.
   */
  class SequencedEvent[T](ev: Event[T]) extends EventNode[T] {

    lazy val onEvt = (id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)]) => {
      // simply asynchronously delegate
      async ! OnEvent(ev, id, v, reacts)
    }

    def deploy {
      ev += onEvt
    }

    def undeploy {
      ev -= onEvt
    }
  }

  type Trace = List[Event[_]]
  case class OnEvent[T](ev: Event[T], id: Int, v: T, reacts: ListBuffer[(() => Unit, Trace)])

}

// vim: set ts=4 sw=4 et:
