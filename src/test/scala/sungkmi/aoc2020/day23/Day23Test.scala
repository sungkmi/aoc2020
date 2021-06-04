package sungkmi.aoc2020.day23

class Day23Test extends munit.FunSuite {

  val testInput = """389125467"""

  val cups1 = CupCircle(Vector(3,8,9,1,2,5,4,6,7), 3)
  val cups2 = CupCircle(Vector(3,2,8,9,1,5,4,6,7), 2)
  val cups3 = CupCircle(Vector(3,2,5,4,6,7,8,9,1), 5)
  val cups4 = CupCircle(Vector(7,2,5,8,9,1,3,4,6), 8)
  val cups5 = CupCircle(Vector(3,2,5,8,4,6,7,9,1), 4)
  val cups6 = CupCircle(Vector(9,2,5,8,4,1,3,6,7), 1)
  val cups7 = CupCircle(Vector(7,2,5,8,4,1,9,3,6), 9)
  val cups8 = CupCircle(Vector(8,3,6,7,4,1,9,2,5), 2)
  val cups9 = CupCircle(Vector(7,4,1,5,8,3,9,2,6), 6)
  val cups10 = CupCircle(Vector(5,7,4,1,8,3,9,2,6), 5)
  val cupsF = CupCircle(Vector(5,8,3,7,4,1,9,2,6), 8)

  test("day23 parse") {
    assertEquals(CupCircle.parse(testInput), cups1)
  }

  test("day23 CupCircle.move") {
    assertEquals(cups1.move, cups2)
    assertEquals(cups2.move, cups3)
    assertEquals(cups3.move, cups4)
    assertEquals(cups4.move, cups5)
    assertEquals(cups5.move, cups6)
    assertEquals(cups6.move, cups7)
    assertEquals(cups7.move, cups8)
    assertEquals(cups8.move, cups9)
    assertEquals(cups9.move, cups10)
    assertEquals(cups10.move, cupsF)
  }

  test("day23 solve1") {
    assertEquals(solve1(testInput), "67384529")
  }

  //test("day23 solve2") {
  //  assertEquals(solve2("389125467"), BigInt("149245887792"))
  //}
}
