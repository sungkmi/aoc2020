package sungkmi.aoc2020.day16

class Day16Test extends munit.FunSuite {

  val testInput = """class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"""

  test("parse") {
    val expectedRanges: Ranges = Map(
      "class" -> Seq(1 to 3, 5 to 7),
      "row" -> Seq(6 to 11, 33 to 44),
      "seat" -> Seq(13 to 40, 45 to 50),
    )
    val expectedYourTicket = Seq(7, 1, 14)
    val expectedNearbyTickets = Seq(
      Seq(7, 3, 47),
      Seq(40, 4, 50),
      Seq(55, 2, 20),
      Seq(38, 6, 12),
    )

    val (rangeSet, yourTicket, nearbyTickets) = parse(testInput)

    assertEquals(rangeSet, expectedRanges)
    assertEquals(yourTicket, expectedYourTicket)
    assertEquals(nearbyTickets, expectedNearbyTickets)

  }

  test("day16 solve1") {  
    assertEquals(solve1(testInput), 71)
  }

  test("day16 solve2") {  

    val testInput2 = """class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9"""

    val expected = Map("class" -> 12, "row" -> 11, "seat" -> 13)
    assertEquals(solve2(testInput2), expected)
  }
}
