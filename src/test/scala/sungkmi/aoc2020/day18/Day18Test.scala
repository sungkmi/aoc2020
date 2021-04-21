package sungkmi.aoc2020.day18

class Day18Test extends munit.FunSuite {

  val testInputs = IndexedSeq(
    "1 + 2 * 3 + 4 * 5 + 6",
    "1 + (2 * 3) + (4 * (5 + 6))",
    "2 * 3 + (4 * 5)",
    "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2",
  )
  
  val expectedPostfix2 = List(
    Digit(2),
    Digit(3),
    Op.Multiply,
    Digit(4),
    Digit(5),
    Op.Multiply,
    Op.Plus,
  )

  val expectedAnswers = IndexedSeq(
    71, 51, 26, 437, 12240, 13632
  ).map(BigInt(_))

  val expectedAnswers2 = IndexedSeq(
    231, 51, 46, 1445, 669060, 23340
  ).map(BigInt(_))

  test("day18 toPostfix") {
    assertEquals(toPostfix(testInputs(2), precBasic), expectedPostfix2)
  }

  test("day18 evalPostFix") {
    assertEquals(evalPostFix(expectedPostfix2), expectedAnswers(2))
  }

  test("day18 evalBasic") {
    for(i <- 0 until 6) assertEquals(evalBasic(testInputs(i)), expectedAnswers(i))
  }

  test("day18 evalAdvanced") {
    for(i <- 0 until 6) assertEquals(evalAdvanced(testInputs(i)), expectedAnswers2(i))
  }
}
