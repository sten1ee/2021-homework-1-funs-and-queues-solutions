package homework1

import homework1.Functions._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionsTest extends AnyFlatSpec with Matchers {
  "fromDigits" should "return 0 for an empty list" in {
    fromDigits(List.empty, 16) shouldBe 0
  }

  it should "return the digit for a single digit list" in {
    fromDigits(List(12), 16) shouldBe 12
  }

  it should "form a decimal number" in {
    fromDigits(List(1, 2, 3)) shouldBe 123
  }

  it should "form a binary number" in {
    fromDigits(List(0, 1, 1, 0, 1, 0, 0), 2) shouldBe 52
  }

  it should "form a hex number" in {
    fromDigits(List(1, 12, 4), 16) shouldBe 452
  }

  "parseInteger" should "parse a decimal number" in {
    parseInteger("123") shouldBe 123
  }

  it should "parse a binary number" in {
    parseInteger("0110100", 2) shouldBe 52
  }

  it should "parse a hex number" in {
    parseInteger("1C4", 16) shouldBe 452
  }

  it should "parse a 36-base number" in {
    parseInteger("1F5Z0", 36) shouldBe 2387196
  }

  it should "parse a negative number" in {
    parseInteger("-1C4", 16) shouldBe -452
  }

  it should "parse a zero" in {
    parseInteger("0") shouldBe 0
  }

  "zipMap" should "transform two lists" in {
    zipMap(List(1, 2, 3), List(4, 5, 6))(_ * _) shouldBe List(4, 10, 18)
  }

  it should "limit the output length to the shorter list" in {
    zipMap(List(3, 6), List(20, 30, 40))((x, y) => y - x) shouldBe List(17, 24)
    zipMap(List(3, 6, 9), List(20, 30))((x, y) => y - x) shouldBe List(17, 24)
  }

  it should "produce an empty list if a or b is empty" in {
    zipMap(List.empty[Int], List(1, 2, 3))(_ * _) shouldBe List.empty
    zipMap(List(1, 2, 3), List.empty[Int])(_ * _) shouldBe List.empty
    zipMap(List.empty[Int], List.empty[Int])(_ * _) shouldBe List.empty
  }

  "countCoinChangeVariants" should "count the ways to give a change" in {
    countCoinChangeVariants(Set(1, 2, 5), 6) shouldBe 5
    countCoinChangeVariants(Set(3, 8, 15, 20, 50), 495) shouldBe 10005
    countCoinChangeVariants(Set(1, 2, 5), 1000) shouldBe 50401
  }

  it should "produce 1 if the change is 0" in {
    countCoinChangeVariants(Set(1, 2, 5), 0) shouldBe 1
    countCoinChangeVariants(Set.empty, 0) shouldBe 1
  }

  it should "produce 0 if the change is not 0 and the denominations list is empty" in {
    countCoinChangeVariants(Set.empty, 5) shouldBe 0
  }

  it should "produce 0 if the change cannot be achieved with the available denominations" in {
    countCoinChangeVariants(Set(2, 4, 6, 10, 14), 11) shouldBe 0
  }

  def toSetDeep(l: List[List[Int]]): Set[Set[Int]] =
    l.map(_.toSet).toSet

  "combinations" should "generate all possible combinations without repetition" in {
    toSetDeep(combinations(List(1, 2, 3), 2)) shouldBe Set(Set(1, 2), Set(1, 3), Set(2, 3))
    toSetDeep(combinations(List(1, 2, 3, 4, 5), 3)) shouldBe Set(
      Set(1, 2, 3), Set(1, 2, 4), Set(1, 2, 5),
      Set(1, 3, 4), Set(1, 3, 5),
      Set(1, 4, 5),
      Set(2, 3, 4), Set(2, 3, 5), Set(2, 4, 5),
      Set(3, 4, 5)
    )
  }

  it should "produce an empty list if n is not 0 and the list is empty" in {
    toSetDeep(combinations(List(), 1)) shouldBe Set.empty
    toSetDeep(combinations(List(), 2)) shouldBe Set.empty
    toSetDeep(combinations(List(), 10)) shouldBe Set.empty
  }

  it should "produce a single element lists for each original element in the list if n is 1" in {
    toSetDeep(combinations(List(1), 1)) shouldBe Set(Set(1))
    toSetDeep(combinations(List(1, 2, 3), 1)) shouldBe Set(Set(1), Set(2), Set(3))
  }

  it should "produce an empty list if n is bigger than the size of the list" in {
    toSetDeep(combinations(List(1, 2), 3)) shouldBe Set()
    toSetDeep(combinations(List(1, 2, 3), 4)) shouldBe Set()
  }

  it should "produce a single combination if n equals size of the list" in {
    toSetDeep(combinations(List(42), 1)) shouldBe Set(Set(42))
    toSetDeep(combinations(List(3, 2, 1), 3)) shouldBe Set(Set(3, 2, 1))
  }

  type Graph = Map[Int, List[Int]]

  val fullGraph: Graph = Map(
    1 -> List(2, 5, 8),
    2 -> List(1, 3, 6),
    3 -> List(2, 4),
    4 -> List(3),
    5 -> List(6),
    6 -> List(7),
    8 -> List(9)
  ).withDefaultValue(List.empty)

  def toList[A](queue: Queue[A]): List[A] = {
    if (queue.isEmpty) List.empty
    else queue.peek :: toList(queue.pop)
  }

  "bfsTraversal" should "find path starting from root" in {
    val result = bfsTraversal(fullGraph)(1, 6)

    toList(result) shouldBe List(1, 2, 5, 8, 3, 6)
  }

  it should "find path starting from bottom" in {
    val result = bfsTraversal(fullGraph)(4, 6)

    toList(result) shouldBe List(4, 3, 2, 1, 6)
  }

  it should "not go through a node more than once" in {
    val graph = Map(
      1 -> List(2, 3),
      2 -> List(4),
      3 -> List(4),
      4 -> List(5)
    ).withDefaultValue(List.empty)

    val result = bfsTraversal(graph)(1, 5)

    toList(result) shouldBe List(1, 2, 3, 4, 5)
  }

  it should "return the path so far even if the end has not been reached" in {
    val graph = fullGraph ++ Map(
      2 -> List(1, 3),
      5 -> List.empty
    )

    val result = bfsTraversal(graph)(1, 6)

    toList(result) shouldBe List(1, 2, 5, 8, 3, 9, 4)
  }

  it should "return only the start if it doesn't have neighbours" in {
    val result = bfsTraversal((Map.empty: Graph).withDefaultValue(List.empty))(1, 10)

    toList(result) shouldBe List(1)
  }
}
