package sungkmi.aoc2020.day17

class Day17Test extends munit.FunSuite {

  val testInput = """.#.
..#
###
"""
  val state = Set(
    (0, 1, 0),
    (1, 2, 0),
    (2, 0, 0),
    (2, 1, 0),
    (2, 2, 0),
  )

  val state1 = Set(
    (0, 0, -1),
    (1, 2, -1),
    (2, 1, -1),
    (0, 0, 0),
    (0, 2, 0),
    (1, 1, 0),
    (1, 2, 0),
    (2, 1, 0),
    (0, 0, 1),
    (1, 2, 1),
    (2, 1, 1),
  )

  def testParse(s: String): State = s.split("\n\n").toSet.flatMap:
    page =>
      val head::body = page.split("\n").toList
      val z = head.dropWhile(_ != '=').tail.toInt
      parse(body.mkString("\n")).map{ case (x0, y0, z0) => (x0, y0, z)}

  test("parse") {
    assertEquals(parse(testInput), state)
  }

  test("range") {
    val expected = ((0, 0, 0), (2, 2, 0))

    assertEquals(state.range, expected)
  }

  test("testParse") {
    val testInput1 = """z=-1
#..
..#
.#.

z=0
#.#
.##
.#.

z=1
#..
..#
.#.
"""
    assertEquals(testParse(testInput1), state1)
  }

  test("step #1") {
    assertEquals(state.step, state1)
  }

  test("step #2") {
    val testInput2 = """z=-2
.....
.....
..#..
.....
.....

z=-1
..#..
.#..#
....#
.#...
.....

z=0
##...
##...
#....
....#
.###.

z=1
..#..
.#..#
....#
.#...
.....

z=2
.....
.....
..#..
.....
....."""

    assertEquals(state1.step, testParse(testInput2))
  }

  test("step #3") {
    val testInput3 = """z=-2
.......
.......
..##...
..###..
.......
.......
.......

z=-1
..#....
...#...
#......
.....##
.#...#.
..#.#..
...#...

z=0
...#...
.......
#......
.......
.....##
.##.#..
...#...

z=1
..#....
...#...
#......
.....##
.#...#.
..#.#..
...#...

z=2
.......
.......
..##...
..###..
.......
.......
......."""

    assertEquals(state1.step.step, testParse(testInput3))
  }

  test("solve1") {
    assertEquals(solve1(testInput), 112)
  }

  test("solve2") {
    assertEquals(solve2(testInput), 848)
  }
}
