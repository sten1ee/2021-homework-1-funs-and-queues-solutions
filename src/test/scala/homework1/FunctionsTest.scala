package homework1

import homework1.Functions._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionsTest extends AnyFlatSpec with Matchers {
  "fromDigits" should "form a decimal number" in {
    fromDigits(List(1, 2, 3)) shouldBe 123
  }

  it should "form a hex number" in {
    fromDigits(List(1, 12, 4), 16) shouldBe 452
  }

  "parseInteger" should "parse a decimal number" in {
    parseInteger("123") shouldBe 123
  }

  it should "parse a hex number" in {
    parseInteger("1C4", 16) shouldBe 452
  }

  "zipMap" should "transform two lists" in {
    zipMap(List(1, 2, 3), List(4, 5, 6))(_ * _) shouldBe List(4, 10, 18)
  }

  "countCoinChangeVariants" should "count the ways to give a change" in {
    countCoinChangeVariants(Set(1, 2, 5), 6) shouldBe 5
  }

  "combinations" should "generate all possible combinations without repetition" in {
    val actualCombinations = combinations(List(1, 2, 3), 2)

    actualCombinations.map(_.toSet).toSet shouldBe Set(
      Set(1, 2),
      Set(1, 3),
      Set(2, 3)
    )
  }
}
