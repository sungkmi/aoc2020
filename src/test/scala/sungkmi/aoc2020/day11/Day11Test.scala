package sungkmi.aoc2020.day11

class Day11Test extends munit.FunSuite {

  val layouts = Seq("""L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL""",

  """#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##""",

  """#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##""",

  """#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##""",

  """#.#L.L#.##
#LLL#LL.L#
L.L.L..#..
#LLL.##.L#
#.LL.LL.LL
#.LL#L#.##
..L.L.....
#L#LLLL#L#
#.LLLLLL.L
#.#L#L#.##""",

  """#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##""",
  )

  test("Step #1") {
    assertEquals(parse(layouts(0)).step, parse(layouts(1)))
  }

  test("Step #2") {
    assertEquals(parse(layouts(1)).step, parse(layouts(2)))
  }

  test("Step #3") {
    assertEquals(parse(layouts(2)).step, parse(layouts(3)))
  }

  test("Step #4") {
    assertEquals(parse(layouts(3)).step, parse(layouts(4)))
  }

  test("Step #5") {
    assertEquals(parse(layouts(4)).step, parse(layouts(5)))
  }

  test("occupationEndup") {
    assertEquals(parse(layouts(0)).occupationEndup, Some(37))
  }

  test("occupationEndup2") {
    assertEquals(parse(layouts(0)).occupationEndup2, Some(26))
  }
}
