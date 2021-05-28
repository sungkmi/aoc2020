package sungkmi.aoc2020.day22

import scala.collection.immutable.Queue
class Day22Test extends munit.FunSuite {

  val testInput = """Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
"""
  val testDecks = Decks(
    p1 = Queue(9, 2, 6, 3, 1),
    p2 = Queue(5, 8, 4, 7, 10),
  )

  val testDecks1 = Decks(
    p1 = Queue(2, 6, 3, 1, 9, 5),
    p2 = Queue(8, 4, 7, 10),
  )

  val recursiveExample = """Player 1:
43
19

Player 2:
2
29
14
"""

  test("day22 parse") {
    assertEquals(parse(testInput), testDecks)
  }

  test("day22 Decks.step") {
    assertEquals(testDecks.step, Right(testDecks1))
  }

  test("day22 solve1") {
    assertEquals(solve1(testInput), BigInt(306))
  }

  test("day22 solve2") {
    assertEquals(solve2(testInput), BigInt(291))
  }

  test("day22 solve2 recursive case") {
    assertEquals(solve2(recursiveExample), BigInt(43 * 2 + 19 * 1))
  }
}
