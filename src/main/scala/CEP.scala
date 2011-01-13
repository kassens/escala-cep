package scala.events

import scala.collection.mutable.ListBuffer

object time {
  private var current: Int = 0

  /** 
   * Returns the current time used for CEP events.
   */
  protected[events] def now = current

  def add(seconds: Int) {
    assert(seconds > 0)
    current += seconds
  }
}

class JointEventNode[T, U, V](ev1: Event[T], ev2: => Event[U], interval: Int, merge: (T, U) => V) extends EventNode[V] {
  var events1: List[(Int, T)] = Nil
  var events2: List[(Int, U)] = Nil

  /*
   * Reaction to event1
   */
  lazy val onEvt1 = (id: Int, v1: T, reacts: ListBuffer[() => Unit]) => {
    events1 = (time.now + interval, v1) :: events1
    events2 = events2.filter(_._1 >= time.now)
    for ((_, v2) <- events2.reverse) {
      reactions(id, merge(v1, v2), reacts)
    }
  }

  /*
   * Reaction to event2
   */
  lazy val onEvt2 = (id: Int, v2: U, reacts: ListBuffer[() => Unit]) => {
    events2 = (time.now + interval, v2) :: events2
    events1 = events1.filter(_._1 >= time.now)
    for ((_, v1) <- events1.reverse) {
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
