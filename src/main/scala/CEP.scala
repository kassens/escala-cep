package scala.events

import scala.collection.mutable.ListBuffer

class JointEventNode[T, U, V](ev1: Event[T], ev2: => Event[U], merge: (T, U) => V) extends EventNode[V] {
  var events1: List[T] = Nil
  var events2: List[U] = Nil

  /*
  * Reaction to event1
  */
  lazy val onEvt1 = (id: Int, v1: T, reacts: ListBuffer[() => Unit]) => {
    events1 = v1 :: events1
    for (v2 <- events2.reverse) {
      reactions(id, merge(v1, v2), reacts)
    }
  }

  /*
   * Reaction to event2
   */
  lazy val onEvt2 = (id: Int, v2: U, reacts: ListBuffer[() => Unit]) => {
    events2 = v2 :: events2
    for (v1 <- events1.reverse) {
      reactions(id, merge(v1, v2), reacts)
    }
  }

  /*
  * Register to the referenced events
  */
  protected override def deploy {
    ev1 += onEvt1
    ev2 += onEvt2
  }

  /*
  * Unregister from the referenced events
  */
  protected override def undeploy {
    ev1 -= onEvt1
    ev2 -= onEvt2
  }

  override def toString = "(" + ev1 + " join " + ev2 + ")"

}
