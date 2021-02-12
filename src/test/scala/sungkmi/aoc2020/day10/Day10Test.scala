package sungkmi.aoc2020.day10

class Day10Test extends munit.FunSuite {

  val smallInput = """16
10
15
5
1
11
7
19
6
12
4"""

  val largeInput = """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""

  test("countJolts small input") {
    assertEquals(countJolts(parse(smallInput)), (7, 0, 5))
  }

  test("countJolts large input") {
    assertEquals(countJolts(parse(largeInput)), (22, 0, 10))
  }

  test("countWays small input") {
    assertEquals(countWays(parse(smallInput)), BigInt(8))
  }
  
  test("countWays large input") {
    assertEquals(countWays(parse(largeInput)), BigInt(19208))
  }
}
