package sungkmi.aoc2020.day14

class Day14Test extends munit.FunSuite {

  val s = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"""

  test("Mask#fromString") {
    val mask = Mask.fromString("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    val expected = Mask(zero = Set(1), one = Set(6))
    assertEquals(mask, expected)
  }

  test("Mask#apply") {
    val mask = Mask(zero = Set(1), one = Set(6))
    assertEquals(mask(BigInt(11)), BigInt(73))
    assertEquals(mask(BigInt(101)), BigInt(101))
    assertEquals(mask(BigInt(0)), BigInt(64))
  }

  test("parse") {
    import Instruction._
    val expected = Seq(
      SetMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"),
      SetMemory(8, BigInt(11)),
      SetMemory(7, BigInt(101)),
      SetMemory(8, BigInt(0)),
    )
    assertEquals(Instruction.parse(s), expected)
  }

  test("solve1") {
    assertEquals(solve1(s), BigInt(165))
  }

  val s2 = """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"""

  test("solve2") {
    assertEquals(solve2(s2), BigInt(208))
  }
}
