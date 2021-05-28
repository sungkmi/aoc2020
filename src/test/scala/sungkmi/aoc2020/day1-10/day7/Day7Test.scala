package sungkmi.aoc2020.day7

class Day7Test extends munit.FunSuite {
  val input = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""

  val map: Rule = Map(
    "light red" -> Map(
      "bright white" -> 1,
      "muted yellow" -> 2
    ),
    "muted yellow" -> Map(
      "shiny gold" -> 2,
      "faded blue" -> 9
    ),
    "shiny gold" -> Map(
      "dark olive" -> 1,
      "vibrant plum" -> 2
    ),
    "dark orange" -> Map(
      "bright white" -> 3,
      "muted yellow" -> 4
    ),
    "faded blue" -> Map(),
    "vibrant plum" -> Map(
      "faded blue" -> 5,
      "dotted black" -> 6
    ),
    "dotted black" -> Map(),
    "dark olive" -> Map(
      "faded blue" -> 3,
      "dotted black" -> 4
    ),
    "bright white" -> Map(
      "shiny gold" -> 1
    )
  )

  test("parseRule") {
    assertEquals(parseRule(input), map)
  }

  test("reachableFrom") {
    assertEquals(map.reachableFrom("shiny gold").size, 5)
  }

  test("countInner") {
    assertEquals(map.countInner("shiny gold"), 32)
  }

  test("countInner case #2") {
    val input = """shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."""
    val rule = parseRule(input)
    assertEquals(rule.countInner("shiny gold"), 126)
  }
}
