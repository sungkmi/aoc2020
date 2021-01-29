package sungkmi.aoc2020.day8

class Day8Test extends munit.FunSuite {
  val testInput = """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""

  test("run") {
    assertEquals(run(testInput), 5)
  }

  test("run2") {
    assertEquals(run2(testInput), 8)
  }
}
