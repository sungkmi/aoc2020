package sungkmi.aoc2020.day13

class Day13Test extends munit.FunSuite {

  val minTime: Timestamp = 939

  test("waitingTime bus 7") {
    assertEquals(waitingTime(minTime, 7), 945L)
  }

  test("waitingTime bus 13") {
    assertEquals(waitingTime(minTime, 13), 949L)
  }

  test("waitingTime bus 59") {
    assertEquals(waitingTime(minTime, 59), 944L)
  }
  
  test("earliestBus") {
    assertEquals(earliestBus(minTime, Seq(7, 13, 59, 31, 19)), (5L, 59))
  }

  test("parse day13") {
    val testInput = """939
7,13,x,x,59,x,31,19"""

    val expected = (
      939L,
      Seq(Some(7), Some(13), None, None, Some(59), None, Some(31), Some(19))
    )

    assertEquals(parse(testInput), expected)
  }

  test("earliestTimestamp") {
    val buses = Map(7 -> 0, 13 -> 1, 59 -> 4, 31 -> 6, 19 ->7)
 //   assertEquals(earliestTimestamp(buses), BigInt(1068781))
  }
}
