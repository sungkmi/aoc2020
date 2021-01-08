package sungkmi.aoc2020.day5

class Day5Test extends munit.FunSuite {

  test("parse seat") {
    val s = "FBFBBFFRLR"
    val expected = Seat(44,5)

    assertEquals(parseSeat(s), expected)
  }

  test("seat id") {
    val s = "FBFBBFFRLR"
    val expected = 357

    assertEquals(parseSeat(s).id, expected)
  }
}
