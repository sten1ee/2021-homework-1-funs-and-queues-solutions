package homework1

import scala.annotation.tailrec

object FunctionsAlternative {
  def fromDigits(digits: List[Int], radix: Int = 10): Int = {
    @tailrec
    def fromDigits(digits: List[Int], acc: Int): Int = {
      if (digits.isEmpty) acc
      else fromDigits(digits.tail, acc * radix + digits.head)
    }

    fromDigits(digits, 0)
  }

  def parseInteger(integer: String, radix: Int = 10): Int = {
    def toNumericValue(digit: Char) = {
      if ('0' <= digit && digit <= '9') digit - '0'
      else if ('A' <= digit && digit <= 'Z') digit - 'A' + 10
      else throw new IllegalArgumentException("A non-digit found")
    }

    if (integer.nonEmpty && integer.head == '-') -parseInteger(integer.tail, radix)
    else fromDigits(integer.toList.map(toNumericValue), radix)
  }

  def zipMap[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] = {
    @tailrec
    def zipMap(a: List[A], b: List[A], acc: List[B]): List[B] = {
      if (a.isEmpty || b.isEmpty) acc.reverse
      else zipMap(a.tail, b.tail, f(a.head, b.head) :: acc)
    }

    zipMap(a, b, List.empty)
  }

  def memoize[A, B, R](f: (A, B) => R): (A, B) => R = {
    val cache = scala.collection.mutable.Map.empty[(A, B), R]

    (a, b) => cache.getOrElseUpdate((a, b), f(a, b))
  }

  def countCoinChangeVariants(denominations: Set[Int], change: Int): Int = {
    lazy val countCoinChangeVariants: (Set[Int], Int) => Int = memoize { (denominations, change) =>
      if (change == 0) 1
      else if (change < 0 || denominations.isEmpty) 0
      else countCoinChangeVariants(denominations, change - denominations.head) +
        countCoinChangeVariants(denominations.tail, change)
    }

    countCoinChangeVariants(denominations, change)
  }

  def combinations[A](xs: List[A], n: Int): List[List[A]] = {
    if (n == 0) List(Nil)
    else if (xs.isEmpty) Nil
    else combinations(xs.tail, n - 1).map(xs.head :: _) :::
      combinations(xs.tail, n)
  }

  def bfsTraversal(neighbours: Int => List[Int])(start: Int, end: Int): Queue[Int] = {
    @tailrec
    def bfs(toVisit: Queue[Int], visited: Set[Int], path: Queue[Int]): Queue[Int] = {
      if (toVisit.isEmpty) path
      else {
        val current = toVisit.peek

        if (current == end) path.push(end)
        else if (visited(current)) bfs(toVisit.pop, visited, path)
        else bfs(
          toVisit.pop.push(neighbours(current)),
          visited + current,
          path.push(current)
        )
      }
    }

    bfs(Queue(start), Set.empty, Queue.empty)
  }
}
