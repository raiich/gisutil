package signdoubt.gisutil.core

import scala.collection.mutable

class SortedPriorityQueue[V](ordering: Ordering[V]) {
  private var queue = new mutable.PriorityQueue[V]()(ordering)
  private var sorted = false
  private var nextValue: Option[V] = None
  private var lastValue: Option[V] = None

  def isEmpty = !nonEmpty

  def nonEmpty = nextValue match {
    case Some(_) => true
    case None => queue.nonEmpty
  }

  def enqueue(elems: V*) {
    queue.enqueue(elems: _*)
    sorted = false
  }

  def head: V = nextValue match {
    case Some(v) => v
    case None =>
      val head = queue.dequeue()
      nextValue = Option(head)
      head
  }

  def last: V = lastValue match {
    case Some(v) => v
    case None => sort().lastValue.get
  }

  private def sort() = if (sorted) this else fit(size)

  def size = queue.size + nextValue.map(_ => 1).getOrElse(0)

  def fit(fitSize: Int): SortedPriorityQueue[V] = {
    var elem: V = dequeue()
    val old = queue
    queue = new mutable.PriorityQueue[V]()(ordering)
    queue.enqueue(elem)
    for (i <- 1 until fitSize) {
      elem = old.dequeue()
      queue.enqueue(elem)
    }
    lastValue = Option(elem)
    sorted = true
    this
  }

  def dequeue(): V = nextValue match {
    case Some(v) =>
      nextValue = None
      v
    case None => queue.dequeue()
  }

  def dequeueAll = if (sorted) queue else queue.dequeueAll
}
