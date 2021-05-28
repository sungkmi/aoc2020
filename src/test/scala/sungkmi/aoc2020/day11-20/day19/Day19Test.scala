package sungkmi.aoc2020.day19

import Rule._

class Day19Test extends munit.FunSuite {

  val testInput1 = """0: 1 2
1: "a"
2: 1 3 | 3 1
3: "b""""
  
  val rules1: Map[Int, Rule] = Map(
    0 -> And(Seq(Index(1), Index(2))),
    1 -> SingleChar('a'),
    2 -> Or(Seq(And(Seq(Index(1), Index(3))), And(Seq(Index(3), Index(1))))),
    3 -> SingleChar('b'),
  )

  val testInput2 = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b""""

  val rules2: Map[Int, Rule] = Map(
    0 -> And(Seq(Index(4), Index(1), Index(5))),
    1 -> Or(Seq(And(Seq(Index(2), Index(3))), And(Seq(Index(3), Index(2))))),
    2 -> Or(Seq(And(Seq(Index(4), Index(4))), And(Seq(Index(5), Index(5))))),
    3 -> Or(Seq(And(Seq(Index(4), Index(5))), And(Seq(Index(5), Index(4))))),
    4 -> SingleChar('a'),
    5 -> SingleChar('b'),
  )

  val testInput3 = """42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"""

/*
  test("day19 parseRules") {
    assertEquals(parseRules(testInput1), rules1)
    assertEquals(parseRules(testInput2), rules2)
  }

  test("day19 match") {
    assertEquals(rules2 `match` "ababbb", true)
    assertEquals(rules2 `match` "abbbab", true)
    assertEquals(rules2 `match` "bababa", false)
    assertEquals(rules2 `match` "aaabbb", false)
    assertEquals(rules2 `match` "aaaabbb", false)
  }

  test("day19 solve1") {
    assertEquals(solve1(testInput3), 3)
  }*/

//  test("day19 solve2") {
//    val (rules, messages) = parse(testInput3)
 //   assertEquals(modifyRule(rules) `match` "babbbbaabbbbbabbbbbbaabaaabaaa", true)
//    assertEquals(solve2(testInput3), 12)
//  }
}
