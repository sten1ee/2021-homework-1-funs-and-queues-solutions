package stj


object Homework1 extends App {
  def fromDigits(digits: List[Long], radix: Int = 10): Long = {
    def fromDigitsEx(digits: List[Long]): (Long, Long) = {
      if (digits.isEmpty) {
        (0L, 1L)
      } else {
        val (num, factor) = fromDigitsEx(digits.tail)
        (digits.head * factor + num, radix * factor)
      }
    }

    fromDigitsEx(digits)._1
  }

  println(fromDigits(List(7,6,0,6,0,3,6,3,2,4)))
  println (Char.char2int('6'))

  def parseInteger2[A](integer: String, radix: Int = 10): Long = {
    def char2digit(c: Char): Int =
      if ('0' <= c && c <= '9')
        c - '0'
      else if ('A' <= c && c <= 'Z')
        c - 'A' + 10
      else
        throw new NumberFormatException(s"Not a quasi digit - $c");

    val chars = integer.toList
    val (sign, digits) =
      if (chars.head == '-')
        (-1, chars.tail)
      else
        (+1, chars)

    sign * digits.foldLeft[Long](0)((num, ch) => num * radix + char2digit(ch))
  }

  println(parseInteger2("-7606036324"))

  def zipMap[A, B, R](a: List[A], b: List[B], f: (A, B) => R): List[R] =
    if (a.isEmpty || b.isEmpty)
      List.empty
    else
      f(a.head, b.head) :: zipMap(a.tail, b.tail, f)

  def countCoinChangeVariants(coins: Set[Int], amount: Int): Int =
    if (amount == 0)
      1
    else if (amount < 0 || coins.isEmpty)
      0
    else
      countCoinChangeVariants(coins, amount - coins.head) +
        countCoinChangeVariants(coins.tail, amount)

  def combinations[T](elements: List[T], klass: Int): List[List[T]] =
    if (klass == 0)
      List(List.empty)
    else if (elements.isEmpty)
      List.empty
    else {
      val list1 = combinations(elements.tail, klass - 1).map(l => elements.head :: l)
      val list2 = combinations(elements.tail, klass)
      list1 ::: list2
    }

  for (combi <- combinations(List(1, 2, 3), 2))
    println(combi)


  /** Queue */
  abstract class Queue[+T](val size: Int) {
    def peek: T

    def push[S>:T](a: S): Queue[S]

    def pop: Queue[T]

    def isEmpty: Boolean
  }

  /** case EmptyQueue */
  private case object EmptyQueue extends Queue[Nothing](0) {
    def peek = throw new Queue.EmptyException()

    def push[T](a: T): Queue[T] = new QueueNode[T](a, this)

    def pop: Queue[Nothing] = throw new Queue.EmptyException()

    def isEmpty: Boolean = true
  }

  /** case QueueNode */
  private case class QueueNode[+T] (element: T, restQ: Queue[T]) extends Queue[T](1 + restQ.size) {
    def peek: T = element

    def push[S>:T](a: S): Queue[S] = new QueueNode[S](element, restQ.push(a))

    def pop: Queue[T] = restQ

    def isEmpty: Boolean = false
  }

  object Queue {
    class EmptyException extends java.lang.Exception

    def empty[T]: Queue[T] = EmptyQueue

    def apply[T](xs: T*): Queue[T] = {
      var qBuilder: Queue[T] = EmptyQueue
      for (x <- xs.reverse)
        qBuilder = new QueueNode[T](x, qBuilder)
      qBuilder
    }
  }

  var q = Queue(1, 2, 3)
  q = q.push(7).push(8).push(9)

  while (!q.isEmpty) {
    println(q.peek)
    q = q.pop
  }
}
