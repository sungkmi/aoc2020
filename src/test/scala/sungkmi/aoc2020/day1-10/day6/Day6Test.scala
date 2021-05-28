package sungkmi.aoc2020.day6

class Day6Test extends munit.FunSuite {
  val input = """abc

a
b
c

ab
ac

a
a
a
a

b"""

  test("sumOfCount") {
    assertEquals(sumOfCount(input), 11)
  }

  test("sumOfCount2") {
    assertEquals(sumOfCount2(input), 6)
  }
}
