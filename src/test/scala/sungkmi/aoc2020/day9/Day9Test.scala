package sungkmi.aoc2020.day9

class Day9Test extends munit.FunSuite {

  val numbers = List(
    35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576
  ).map(BigInt(_))

  test("findFirstInvalid") {
    assertEquals(findFirstInvalid(5, numbers), Some(BigInt(127)))
  }

  test("findRange") {
    val expected = Seq(15, 25, 47, 40).map(BigInt(_))
    assertEquals(findRange(numbers.toIndexedSeq, BigInt(127)), Some(expected))
  }
}
