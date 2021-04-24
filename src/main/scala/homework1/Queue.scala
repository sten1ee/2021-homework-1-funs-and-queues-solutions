package homework1

class Queue[A] private (front: List[A], back: List[A]) extends Iterable[A] {
  def peek: A =
    if (front.isEmpty) back.last
    else front.head

  def push(a: A): Queue[A] = new Queue(front, a :: back)
  def push(as: Seq[A]) = new Queue(front, as.toList.reverse ::: back)

  def pop: Queue[A] =
    if (isEmpty) throw new NoSuchElementException
    else if (front.isEmpty) new Queue(back.reverse, List.empty).pop
    else new Queue(front.tail, back)

  override def isEmpty: Boolean = front.isEmpty && back.isEmpty
  override def size: Int = front.size + back.size

  def iterator: Iterator[A] = front.iterator ++ back.reverseIterator
}

object Queue {
  def empty[A]: Queue[A] = Queue()

  def apply[A](xs: A*): Queue[A] = new Queue(xs.toList, List.empty)
}
