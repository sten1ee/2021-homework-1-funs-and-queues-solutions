package homework1

object Functions {
  def fromDigits(digits: List[Int], radix: Int = 10): Int = ???

  def parseInteger(integer: String, radix: Int = 10): Int = ???

  def zipMap[A, B](a: List[A], b: List[A])(f: (A, A) => B): List[B] = ???

  def countCoinChangeVariants(denominations: Set[Int], change: Int): Int = ???

  def combinations[A](xs: List[A], n: Int): List[List[A]] = ???

  def bfsTraversal(neighbours: Int => List[Int])(start: Int, end: Int): Queue[Int] = ???
}
