package sungkmi.aoc2020.day15

class Day15Test extends munit.FunSuite {

  test("solve1") {
    assertEquals(solve1("0,3,6"), 436)
    assertEquals(solve1("1,3,2"), 1)
    assertEquals(solve1("2,1,3"), 10)
    assertEquals(solve1("1,2,3"), 27)
    assertEquals(solve1("2,3,1"), 78)
    assertEquals(solve1("3,2,1"), 438)
    assertEquals(solve1("3,1,2"), 1836)
  }

//  test("solve2") {
//    assertEquals(solve2("0,3,6"), 175594)
//    assertEquals(solve2("1,3,2"), 2578)
//    assertEquals(solve2("2,1,3"), 3544142)
//    assertEquals(solve2("1,2,3"), 261214)
//    assertEquals(solve2("2,3,1"), 6895259)
//    assertEquals(solve2("3,2,1"), 18)
//    assertEquals(solve2("3,1,2"), 362)
//  }
}
