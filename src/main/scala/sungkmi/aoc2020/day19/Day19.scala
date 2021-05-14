package sungkmi.aoc2020.day19

enum Rule:
  case SingleChar(char: Char) extends Rule
  case Index(n: Int) extends Rule
  case And(rules: Seq[Rule]) extends Rule
  case Or(rules: Seq[Rule]) extends Rule

import Rule._

def parse(s: String): (Map[Int, Rule], Seq[String]) =
  val Array(ruleString, messages) = s `split` "\n\n"
  (parseRules(ruleString), messages.split("\n").toSeq)

def parseRules(s: String): Map[Int, Rule] = s.split("\n").map:
  line =>
    val Array(i, body) = line.split(":")
    i.toInt -> parseBody(body)
.toMap

def parseBody(body: String): Rule = body.trim match
  case s if s `contains` '|' =>
    val subBodies = s `split` '|'
    Or(subBodies.toSeq `map` parseBody)
  case s if s.startsWith('"'.toString) && s.endsWith('"'.toString) && (s.size == 3) =>
    SingleChar(s.tail.head)
  case s =>
    val indexes = s `split` ' '
    And(indexes.toSeq.map(_.toInt).map(Index(_)))

extension (rules: Map[Int, Rule])
  def `match`(s: String): Boolean =
    def matchRemainder(rule: Rule)(s: String): Option[String] =
      println(s"===> $rule: $s(${s.size})")
      rule match
        case SingleChar(ch) => if s.headOption == Some(ch) then Some(s.tail) else None
        case Index(n) => matchRemainder(rules(n))(s)
        case And(rules) => andLoop(rules.toList, Option(s))
        case Or(rules) => orLoop(rules.toList, s)
  
    @annotation.tailrec
    def andLoop(rules: List[Rule], s: Option[String]): Option[String] = (rules, s) match
      case (_, None) => None
      case (Nil, s) => s
      case (x :: xs, Some(s1)) => andLoop(xs, matchRemainder(x)(s1))

    @annotation.tailrec
    def orLoop(rules: List[Rule], s: String): Option[String] = rules match
      case Nil => None
      case x :: xs => matchRemainder(x)(s) match
        case Some(s1) => Some(s1)
        case None => orLoop(xs, s)

    val ans = matchRemainder(rules(0))(s) == Some("")
    if ans then println(s)
    ans

def modifyRule(rules: Map[Int, Rule]): Map[Int, Rule] =
  rules
    .updated(8, Or(Seq(
      Index(42),
      And(Seq(Index(42), Index(8)),
    ))))
    .updated(11, Or(Seq(
      And(Seq(Index(42), Index(31))),
      And(Seq(Index(42), Index(11), Index(31))),
    )))

def solve1(input: String): Int =
  val (rules, messages) = parse(input)
  messages.count(rules.`match`)

def solve2(input: String): Int = 
  val (rules, messages) = parse(input)
  messages.count(modifyRule(rules).`match`)

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit = 
  val ans = solve2(input)
  println(ans)

lazy val input: String = """101: 118 64
83: 132 118 | 108 2
61: 19 2
33: 126 2 | 68 118
80: 2 48 | 118 91
47: 38 2 | 64 118
40: 25 2 | 93 118
18: 38 2 | 108 118
2: "a"
74: 118 108 | 2 12
41: 118 17 | 2 58
56: 125 2 | 132 118
66: 12 2 | 27 118
62: 118 52 | 2 125
58: 2 128 | 118 99
89: 110 2 | 3 118
30: 118 85 | 2 21
20: 2 70 | 118 111
85: 118 115 | 2 52
98: 118 46 | 2 10
12: 118 2 | 43 118
99: 102 118 | 56 2
127: 2 119 | 118 100
53: 125 2 | 108 118
15: 2 27 | 118 108
22: 95 2 | 61 118
109: 115 2 | 46 118
90: 76 2 | 121 118
37: 15 118 | 23 2
27: 118 2 | 118 118
104: 2 46 | 118 12
32: 64 2
26: 35 118 | 32 2
123: 131 2 | 89 118
21: 118 132 | 2 115
95: 112 118 | 125 2
79: 88 2 | 60 118
34: 107 2 | 133 118
6: 2 101 | 118 110
55: 118 112 | 2 64
10: 2 118
115: 2 118 | 2 2
39: 2 61 | 118 81
42: 92 118 | 124 2
105: 38 2 | 10 118
7: 10 2 | 27 118
106: 87 2 | 1 118
75: 118 46
28: 2 71 | 118 16
120: 118 27 | 2 112
100: 69 2 | 109 118
102: 46 118 | 132 2
5: 39 118 | 51 2
14: 57 2 | 96 118
8: 42
25: 118 59 | 2 67
3: 19 118 | 52 2
59: 2 74 | 118 83
87: 118 72 | 2 78
130: 111 2 | 117 118
112: 2 43 | 118 118
116: 118 38 | 2 108
17: 2 122 | 118 29
124: 41 118 | 40 2
11: 42 31
38: 43 43
44: 118 123 | 2 114
23: 12 2
29: 2 113 | 118 129
121: 2 10 | 118 19
93: 2 26 | 118 36
13: 19 118 | 46 2
107: 118 132 | 2 10
54: 118 27 | 2 12
63: 2 105 | 118 82
84: 2 37 | 118 73
52: 118 2 | 2 118
50: 118 111 | 2 9
111: 118 112 | 2 27
82: 118 46 | 2 108
118: "b"
133: 115 2
103: 34 2 | 22 118
36: 66 2 | 65 118
31: 33 118 | 79 2
69: 12 2 | 115 118
122: 54 118 | 66 2
114: 6 118 | 80 2
92: 118 14 | 2 44
0: 8 11
117: 132 2 | 108 118
76: 64 2 | 27 118
126: 2 28 | 118 127
113: 125 2 | 19 118
43: 118 | 2
46: 118 118 | 2 118
132: 118 118 | 2 2
49: 118 130 | 2 50
129: 19 118 | 27 2
24: 2 27 | 118 52
119: 118 47 | 2 98
91: 19 118 | 10 2
35: 115 118 | 46 2
57: 2 63 | 118 90
125: 118 118 | 43 2
108: 118 118
4: 115 118 | 19 2
94: 112 2 | 115 118
1: 2 55 | 118 75
9: 38 118 | 112 2
71: 2 18 | 118 53
60: 103 2 | 86 118
51: 116 2 | 24 118
128: 24 118 | 117 2
45: 118 2
16: 104 2 | 62 118
70: 118 115 | 2 45
77: 118 13 | 2 94
131: 61 2 | 107 118
68: 2 49 | 118 84
86: 118 97 | 2 30
78: 118 38 | 2 12
88: 5 2 | 106 118
81: 2 45 | 118 52
64: 2 2
19: 2 43 | 118 2
110: 132 2 | 64 118
96: 2 77 | 118 20
67: 104 118 | 4 2
65: 115 118 | 38 2
73: 118 110 | 2 120
72: 64 2 | 115 118
48: 2 46 | 118 10
97: 7 118 | 133 2

aaaaaababababbbbbababaab
aaababbabbbbbbabaaaabbaaabababaabababaaa
abaabababaababbaaaabbaaabaabbbaa
aabbaaaaabbbaabbaabbbabbbabaabbbaaabaabababaaabb
bababbbbbaaaaaababbbabbb
bababbaabbaaaaabbaabbabbbabbabbbbbbbbbababbbabab
aaabaababbaabbbbaaabaaabaabababb
babaaababbbaabaaaabababa
aaababbabbbbbaabaababbaabbababbbabaabbbbabababaaaaabbaab
baabbbbabaabaababbbbbabaabaabaabbaaaabab
bbbbaaabbabbbbabaaaabbabbbbbabaaaaaabbaa
bbababbbaabbaabaababbbab
bbaabbbabbbaababbbabbabb
abbaabbaaabbbaaaaabaabababbaabbababaabbbaaaabbaa
bbaababbbbbbbaabbbabaaab
baabbbaabbababbbababaaabaabbaabb
baaabbbababbbabbabbaabaa
bbaaabaabbbababbbbbbbabb
aabaabbbabaaaaabbbbaaaaa
babbbaaababbbbaaaabaaabbbabaabba
abbabbbbaabbaaaaabbbbbaabbbbbabbbaabaabb
aabbbbaaaabaaabbaabaaababbbbbabbbbaababa
abaaaabaabbaaabaabaaaabbabaaabaaaaabaaab
babaabbbbaaabaabbbababbbaabbababaaaaabababbbbaababbbbbbb
ababaaababaaaabbbababaab
bbbbabaabbbabbabbabbabbaaaabbaab
abababbbaaabababbbbbabbbaabbbabbbabaabaababbbbabaababbbb
bbaaaababaabbaabaabbababaabbbbba
abbaabaaabaaabaabababbbbbbaabaaaabbbabababbababbbabaabaabbabaaaa
bbbbaabaaaabaabbabababab
abbabbaabbbbbababbaabbbb
bbbbaabbababbbbbbbbbaabaaabbabaaaabaaaba
bbbbbaaaabbaabbbaaabbaaaabaabbaabbbbbabb
bbaaabaabbbbbbbbabbbbbaa
baabbbbaaababbababbaaabb
aaaababbbbaababaabbbbbbbaababbbb
aaaaabababaaaababbbaaaaa
abbbaabbbababbaaabaaabaa
bbbbbababbbbaabbaabaaaba
aabbbabbbbbababbaaaabbba
aabaaaaabbaaaabababaabab
bbbbbaaabbbabbabaabaaaab
bbbabbbaaabbbabbaabaaabbbbabbababbabaabbbbababab
abaababbabaabbaaaabbbbbababbbbaaababaaaabbbbabba
abaababbbbbababbabaaaabbababababaaabaaab
baaabbabaabbbabbaaaabbabbabababbbbbabaabaababbbbbababbba
bababbabbbbbbbaaaabbbabaabaaababbbbaaaaa
baabaaabaaabaaaabbaaaabb
bbbabbbbbabbbbbaababbaab
bbaaaabaaabaababbaaaaabbbbaaaabb
abbabbaabaaaaaabbaababaa
aaaaaaaaabaaaaabaaaabbabaabababa
bbbbaabaabbabbaabaabaabb
bbbbabbababaabbbabaaabbb
aaabaabbaabbaaaabbbaaaaa
abbabaaababababbbbbbaabaabbbaabbaaabaaaaaabbabaaabaabaaa
baaaabbbbabaaabaabaababbabaaaaaaabbabaaabbabbaab
abbbaabababbbaaababbbbaaabaabbbbbaaaaaababbabaaaabbbbbaabbbabababbabbaaa
aaaaaaaaaabbbaaabbabbaab
bbabbbaabbabbbbabbaababaaaabbabaaaabaaba
abbaaaaabbaaaaabbbaababbaabbbababababbbabbabbbbabbbaaaabaabaabab
bbbabaabaaabbaaabaabaaabbaabbbbb
aaabbaaabbabababababbaabbabaaaaabbbaabbbbabaabab
abaabbbbabbbbaaabaaaabab
abbaababbbbababbababaaaa
bbbbbaabbbabbabaababaaaa
bbbaabbaabbbbbabaabbbabbabbbaababaababbbaaaaabbbbaaaaaba
aabbbabbbaabbaaaaabbaabb
aaabbaaaabaaaabaabbbabab
bbbbaababbbbbaabaaaaababbabbbaababbabaab
baabbbbabaabbaabbabaabaaabaabbabaaabaaaaababbbabaababaaa
bbbbaaaabbbabbbbabbbbbaa
abaababbbbbbababbababababaabaaababbbbbaa
aaabaaaabbaabaaaabbbabaa
bbbabbabaabbababbaabbbaababbaaaaabbabaab
aabaaabbabbbbabaabaabaaa
bbbbaabbabababbbaaaaaababababaaabbbababa
bbbabbbabbabaabbaaabbbbb
abaaabbabaaababbaaabbbab
bbbbabaabaaabbbbaaaabbabbbaaabaababbbaab
abbaaaabaababbabaabaababbbabbaab
aabbabbaaaabbbbababaaaab
abbbabbaabbbbababbbbbaabbababbabaaabaaabababbbab
bbabaabaabaaaabababbbbbb
bbbbbbbbaaababbaabbbaaab
bbaabbababbbbaaabbaaaabb
babbabbbbaabbabbababaaba
baaabaabaaabbbbaababbbbaaaabbbaaaaabbbaabbbababaaabbbaab
aaaaabababbabaaabaaabbbaabababababaabbba
baaaabbbaaabbaaabaababbabababbababbabbaabaababbaaaabbabaaababaaa
babbbaaaabbaabbbbabbbbbb
bbbbbaaabbaabbbabaaaabba
abbbabaabbbbabbaabaabaaabaaaabbabbbaaaabbbaabbabababbaabbaaaabaa
babbababaababbababbababa
abaabbbbabbaabababbaaaaa
bbbabbaaaaabababbbaabbaabbababaabbbabaaa
abaaaabaababbbbbaabaaaaabbbabbbabbababab
aababbababbaabbbbbbbbabb
bbababbbaabaaaaabbababbabababbabbbabbaaa
abbbbabbbababbbaaaaabbabbbabbbabaaabbbaabbbaabbaaaaaabba
ababaaabababbbbaaaaabbababbbbaab
baabbbbabaaabaaaabaababaaaabbaba
baababbabbbbbabbbaabaabb
bbbababbbababbaaaabaabbbbbbbbabaabbaaabb
baabaabaababbaaaababaaabbbbbbbba
babbaababbbbababbaabaabb
aabbbabaaabbaaaaabbaaabababaabba
bababbbbbaabaaabbbaababa
bbbababbbaabbabbabbbaabaabababaa
bababbaaaabaabbbbbbbaaab
bababbabaabbaaaabbbaaaba
baabbaabaabaabababaaaaaababaabab
baabaaabbbaabaaabbbbbbab
abbbbbbabbaabbbabbaabbbaabbaabbababbaabb
babbbaaaabaabbbbaabbaabb
baabbbababbbababbabaaaaabbababab
babababbaaabababbbbababbaaaababa
abbbbaaaaabbabbaabbaaaaa
bbbbabaababbbbaaaaaaaaab
abbabbaabbbbaabbbabaabbbbaaabaaaaaaabaabbbbbbbaa
bababbbbbbababaaaabaabba
bbbbbaabbbabaababaaaabab
aaabaabbbbaababbaababbbaabbbbbbabababbab
aaaaaaaaabaababbbbabbaab
abababbbabbabaaabaaabbaabbaaaabb
babbbaaabaabaaaaabbbaabaabbbbbbaaaabbabbabbbabab
babbabbbbbbbaabbaaaabaaa
baabbbbaabbbabbabbaabaab
bbbbabbabbababbabbbabbabbabbbaaaaaaabbabaaaaaabb
abbaabbbababbaaabaaaabaa
babbbaaababababbbaabbbbb
bababbaaaaababbaaabbaabb
bbbbabbbbbaabbaaabbbabbabababbbbbababbabbabbabba
abbabaaabbaabbaababbbbaaaaababbaabbababb
baababbabbbbababbbbabbabbbbbbaaaabbaabba
babbbbbaabbabbbbabbaabba
baaaabbbabaabbbbbaaaaabbbbbabbabaaaabbbabbaaabab
abbabbbbbbbaababababaaaa
baaabaabbbbbababbabaabbbbbbbbbbabbabbaab
bbbbbaabbabaaabaaababbabbabbbbabaabaabaa
bbaaaaababbbbabababbabbbaaababbb
aababbbaabbbaabbabaababababaaaab
bbbabbbbaaaaababaabbbbba
bbabaabaabbbabbabbbbbaabbbbbabababaaaababbbaaaabbbbbbabb
bbbbbabaabbbbababbababab
bbbaabbaabbbbbbabbbabbbaabbbbaaaabababab
baaabaaaabaaaabaaababbaabaabbabbababbbbabbaaabab
baaabaaaaaabaaaaabbbbabb
abaaaababbabbabaabaaaaaababbaaaaabbbbabb
baaabbbababababbaaaabbbb
aaabbbaaabaaabbababaabbbbbaabaabbbaaabbb
bbbababbaaababababbbbaaaaaabaabbbaabbbbb
abaabbbbababbbbaabbaabbbaabaababbbbaaaba
aabbbabbabaabababaabaaabbbbabbaaaaabbbbbbaaaabab
aaaaabaaabaababababbbbbb
bbbbababbaabbbbababababaabbbaaab
baaabbbbaaabbaaababbaabb
aabbbabbbbbbbaababbabbab
babaabaaabbbbbbaabaaaabbbbaabaaaaabbaabaaabbabbb
baaaabbbaabaabbbaaaababb
aabbababaaabababbabbabbaaabaabaaaababaaa
abbbbbabbaabaaababbbaabbabbaabababababbbaaaabbabaabbbbbabbabbbaabbbaaaba
baababbaababbaaaaaaabbba
bbbbaabbbaaaabbbbaaaaaaa
bbbbababaaaaaabaaaababbb
aabaaaaabbaaaaabbbbababbbbbaabbabbbbbabb
bbbbaababaabbabbabbbaabbbbbbaabaabaabbbbabbababbbbbaaaaa
abbaaaabbbabbabaababbaaaaababbabababbbabababbabb
bbababbbbaaaabbbabbbbbaa
bbaabbaabbbbabaaabbbbaaababababbabbbbbaabbaabaab
bbaabbbabbbbbaaaaabbabaabaaabbbbababbbbb
baababbabbbababbbbbbbbab
abbbbbbaaaabbaaaaaaaabba
bbaabbbababbabbaaaaabaab
bbbaabaababbbbbabbabbaaa
bbababbbabbbaabbbaabbbbb
bbaaaabaaaabaaaaaabbabbabaaabbaabbbababa
aaabababaaabbbbaabaaabbb
bbaababbaabbabbabaababbb
babbbabbaabbabbaaaabbaaabbbaabaababaaabbaabbbbabbbaaaabb
babbbbababbbaababaababbb
bbbbabbabababababbbbaabaaabaabbbabbaabbbbbaabbaabbabaaaabbbaaaba
aabaaaaaabbaaaabbbbababbbbaaabaabbaabbabbbabbbaa
bababbaabaaabaaabababbbbbbaaabbbabbababa
aaabbbbaaaaaaaaabbbaaabb
bbbbabbabbbbaabbbaaabbaabbbaaaabbaaaabab
babaabaabaaaabbbbaaaabba
abbbaabbbabbabbbbaaabaaaabbabbaabbbaaabb
bbbbaabbaababbabaaabbaaaabaabbaaabbabbab
abbbbaaaabbbaabaabaaaaaabbaaaabaaabababa
baaabaaaabaaaaababbabbbbbababbba
baabaaaabbaaaaabaaababbaababaabbaabaabaa
baaababbbaabbaababbbabaa
babbbbaaaaababbababbbbbb
bbaabaaaaabbabbabaaaabbbaabaaabbbbabaaaaabbababa
baabbbaaaabbaaaabbaaaabb
baabbbbabaabaaabaabaabaa
bbbabbbbbbbabbaaababaaababbabaab
aaaaaabaaabbbbaaabaaabaa
ababbbbaabbaababaababbbb
abaaaaababbbbbabbbbbbbba
aababbbabbaaaaababbbbbaabbabaaabaaababaa
aaabaaaaabbbabbaabaaabbb
bbaababbbbbbaabaabbaaaaa
bbababbbbababbaabaaabbbbabaaaaababbbbaabaaaabbba
abbabaaabbaabaaaababaaaa
aaabaabbabaaaababaaabaaaaabbbabbbabbbbabbbabbababaabbbbbaababababababaab
abaabababbababbbbbbaaaba
abbbbababbaabbbaaaababaa
aaaaabaaaababbbaaaaabbba
abbaababbbbabaababaababbbbabaabbbbbbbbaa
abbbaaaabbbbabbabbaaaaaabaaabaababbaaabbababaaaaaabbaaabbbabbaaaaababababababbbbbaaaabab
ababbaaababaabbbbabaaababbbaabbb
bbbaabbabbbaabaaaabbbbaaaaaaabaaaababbabbbabbbbaabaabbaabbbababa
bbbbaabbabbbbabababaaaab
aaabbaaaaabaaaaababbbbbabbabbbab
bbbbaaaaabbbbbbabbaababa
abaaaabbbbababbabaaaabbbbabbbbbabbababaabbabbbabaabaabbabababbba
baaabbbabbbabbaabbabaaab
aaababababaabbbbbabaaaab
bbaaabaaaabaabbbbabbbabababaaaaa
abababaaaabbbbaabbabbbaababbbbaaaaaaaabbabababbbbaaaabaa
baabbbbabaababbabbabbbba
baabbbaaaabaabbbabababab
abaababbbbbababbaababbabababbbab
baaabaabbabababaaaaaaaaaabbabbbbbaaaabba
baaaaabbbaaabbbababaaaaa
baabbaaabbaabbabbabbbbbb
babbababaaabababaabbaabb
bbbabbbbaabbabbabbbaaaaa
bbbabbabababbaaabbbaaabb
aaabababababbaaabbabababbaaabbabaaaaabbbbabbbbaaaaabbaab
aabbbbaabaaaabbbbbabbbaa
bbaabbaaaaaaaababbaabbaabbaabaaabaaaaaaaababbaab
bababbaabbaaaaabbabaabbaababababaaaaababaabbaabbbbabaaabbaaaabbabaabbbbaabbabbbb
baabaababbbbaaaababbbbbb
baaabbaaabbbaabaaaaaabaabbbabbababbaabbbaaabaaaaaaaabababbbaabbbbbbaaaabababbabbbababaab
aaababbabbaabaaaaabaaaab
bbbabbabbbbbababaabbabbababababbaababbbaaababaaa
babbbabbbbaaaaabbbaaabaabbbaaaaa
bbaaabbabbaaabaaaaaabaab
bababbbbbaabbaaabaaabaaabababbabbaabaabaabaaabaa
bbbbbabababbbbaaaaabababaaaabbabaabbbbab
bbbbaaaaaababbbaabaaaabbaabbaabb
babaabaabbaaaaabbaabaabababbbbbaababbaaabaaaabbbbbbbbbabaabaabaa
babbbabaaaaaaaaaaababaaa
bababbabbabbbbbaaaababbb
abbaaababbaaaababaaababa
bababbaabaaababbbbbbaaaabbbaabababbabbba
baaabbbbaabbbaaaaaaabbababaaaaaaabbabbababbbaaaaababaabb
babbbabbbbababbaabbbaaaabaaabbbabbababbabbbbbaabaaaaababaaabaaaa
ababbbbbbbbbabbaaabaabaa
bababbaababbbbaaaaababababaaaaabbbaaabbb
bbbbbaabbbaabbaaaaabababbaababab
aabaaabbabbabbbbaabaaaaaaaabbbbb
bbaabbabbbbbabbaababaaba
aaabbbbaaabbabbababaabbbaabbababbabbaaabaabbbbba
babbbbabaaaabbabbbbbbabb
babbbabaaabaaaaaaabbbbaababbaabaaababaababbaaaaa
aaabaaaaaabbabbaaabbbabaabbaaabaabbaaaaa
babababbbbbbabaaabbbbbbaababaaaaaabaabaa
aabbabbababbbaaabbbaabbb
aabbaaaaabbaaaaaaaabbbaaaaababbbbbbabbabbbbabaabababaaaaababbbaaaabbbaab
bbbabbaabbbbaaaaabaabbba
babbbabbaabaabbbaabbabbb
abaaabbabaaaabbbaaaaabaaababaaabbabbbabaabaaabababababab
aabbaaaabbbbbaaababbaabababbaababbaaaabaabbabbbaabbaaabbbbabababbaaaaaaa
bbbbbbbbbbaaaababbbbbabbaaabaaababbbababaaabaaba
aabbbababaabbaaaabbaabba
bbbbbaabbbbaabbabbbbbababaabbbbbaaaaabba
abbabababbbabaababaaaabbbbbaaabbbaabababbbaabbbabbaabababbabbbba
aabbbabbababbbbbaaabbbab
bbbbbaaabbbababbbbbabbbaaaabbababbabbbaa
bbbabaababbaabbbabbbbabb
bbaabbbaaabbababbababaaa
abbabbbbbbaaabbaabbbabbb
abbbbaaaaabbaabaabbaabba
bbababaabbbabaabaababaabbbaaaaabaababbaababbaabbaaabbbabbabaaaaa
baaabbaabbbabbabbaabaabb
bbbabbbbabbbbabaababaaababbabbbabbababab
bbbbbaaababaaabaaabbbababbababababbaabba
baabbaababbbabbabaabbabbabbbabaaaaabbabb
bbbbbaaabbbabbaabbabbaab
babbbababbaaaaabbabbbbbb
abaaaabaaaababbaaabababa
bbbababaaaabbbbabbbbabbaaabbaaaababbaaaababbaaaaabbaaaaa
abbabbaaaaaabbabbbaaabab
bbbaabababaabbbbbabaabab
babbbaaaababbbbbbaaaabba
bbaababbbbbbababbbaabbbb
aabbbbaabaaabbbbababbaaabaaabbababbabbbbbbabaaaa
bbababbabababbaabbaaabbaabaaabbabaaabbaaaaaabaaaababaaaaabbbbbaabbabaaaaaabbabaaaaaabaab
baaabbbbbabbaababbbaaaaabbbaaabaabbaababbaababbbbbbbbbbaaabbabbaabbaabbaaababbbaaaaabbabbbbbaaba
aaaaaaaabaaaaabbbaabbaba
baaaabbbbaabaaaababaaaaa
baaaabbbbaaaaaabaaabbbbabbabaabaababbabaaaaabaabababbbaa
bbbbabbbabaaaabaabbabbaaabaabaaa
baaabbaababbbbbaaabbaabaaabaaabbabaabaaabbabababababaaba
bbaaaababaabaabaaaabbbaaaabbaaaabbbbbabb
bbbbabbbbbababbbbbabaaab
bababababbbbaababaababbaababbbbbaabaabba
bbaabbbaaaaaaababababbba
bbbbaabaabaaaababbabbabb
bbbaabbaabbbbaaaababbabb
abbaaaabbaabbabbaabbbbaabaabbaba
abbbbbbabaaaaababbaaabababbbbabbaabbbbba
babbabbabbbaabaabbbaabaabbaaaaaababbbbbb
abbaaaabababbaaaaaabbbbb
bbaaaababbaaabbaabbbbbbaabbaaaabbbabbaabababbbaabaabbbbb
aababaabababbabbaaabbaababaabbbabaaaabbaabbaaabbbbbbaabbaaababbabababbaaaaabaaba
bbbbaabbbbaabbaaaaababbaabbbaabbbbbabaaa
abbbbababbbaababbabbbaaabaabbaabbbbaaabb
aaaaababbaaabbabaabaabaaaaabaabaabbbabaaabbbabaababaabaa
aabbbbbbabaabaababaaababbabbbabbababaabaaaabbabb
bbaabaaabaaababbaaaaabababbaabbbaaababbbbababaaa
bbabaaabbabbbabaabaaaaaababbbababaaaaaaaaaaabbabbabaabbabbbaaaabaaaababb
baaaabbbbaaabbabbabbbbbabaaababbaabbbabbaaabbbabbbabbbbbaabaabaa
babbaabababbbaaabaababbaababbabbbbbabaaabbabaaababbbabbbbbbababaabbaaaaa
abaabbababaabbabaaaabaaa
bbababbaabbaaababaabbaba
baaabbaaaabbababbaabaabb
aabbbbaabababbabbbabaabaaaaaaabb
bbbabbaabbbbaabaaabababb
aaabbbbabababbaabaaaabab
abababbbbbabaabbbbbaabbabbabbaabbbaaaabb
aaaaaaaaabbaaabaabbaaabb
babbabbaabbbaababbabbbba
aaaaaabaaabaaabbbabababbbbabbabb
babaaabbbbbabaaababbabaabaaaabab
aaababbbababbbaaababbaabbbbbababaababbbaaabaabbabaababbbbbaabbbbbabaaabababbbbaaaaaabbabbbababaa
abaababbaabbaaaaabbbaaaa
abbaaaababbabaaabbaabbabbaabbbaabaababaa
aaaaabaaaabbaaaaabababaa
baaaaaababaaaabaababbaaaaaabbbabbaababababaaabaaabbbbabbaabaaabb
bbbbabaabbbbbaabbabbaabaabbaaabbbbabaaaababbbaab
abbabbbbabbabaaaabbabbba
baaabbbbbabbababaaaabbbb
abaabababaaabbaaaaabaabbabaaaaabbabbababbbaababa
abaaaabaaababaabbabbbbaabaabaabaaaabaaaababaaabbaabbbaabbbbaabbb
bbaaabbabbbababbbaaabbaaaaababbb
bbaaabbabaabaaaabbabbabaaaaaaaaaabbaaaabbbabaabaababbababbbbbbba
aababbbabbababbbabaabbba
aabbbabbbaaabbabbbbbbbbbbbbabaababbaababababbbaaabbaabaa
aaaaaaaaabaaaaaabbbabbaababbbaaaaaabaaab
bbaaabaabbabbabaabbabbaaababbaab
abaaabbaaaabbaaaabaabababbbbbbaaaaabbbbb
baabbaababaababaaabaaabbabaaabaa
bbbabbbabbababbbaaababaa
bbabaabbbbbbaababbabbbbb
abbbbababbbabbaaabbbabab
baaabbbababaabbbbbabbaab
aaaaaaaaaabaaabbababaabb
bababbaaabbaabbbaabbaaab
babbaababbaaaabaabaababbaaaabaaa
bbbabbabaabaaabbaaaabbabababbaaaabaaabbb
aabaaaaababaabaaaaaababb
aabbabababbaababaaaababb
bbbbabbabbbaabbabababbbbbbaaaabababaabbaaaabbbbbaabbbbba
ababaaabababbbbaabbaaabbabbbbaaaaabbbaabbbabbabaabaabaab
aabbababbabbabbbaaabbaba
abaabbabbaabaaaabbbaaabb
babaaabaaabbaababbbabaaa
bababbabbaabbabbaabbaaaaaabaaabbabbabbbbaaabbaba
babbabbaabaabbabaababbbb
babbbbaaabaaabbabbaabbbabbbbabbbbbabaababbbbaaababaabaaabbabbbaaaaaabaab
baabbaabbbbbbaaabbabbbbb
bbbababbbaabbbaabaabbbbabbababbbbaaaaabbaabbbaab
aabbaaaaabbabbaaababbbbbbaabbbbaaaaabbbbbbababab
baabbabbaaabbbaabbbbabababbaaabb
babbabbbbbaaaaabbbaaaaaaaaaabaaabaaaaaaaaabbbbbbbaaaabbbababaabbaabbbaaa
bbbaabbabbabaabaabaabbaa
abbaaaabbbbabbabbaabaaaaabaaaaabbbbabbabbaababaaabaabbbaaabbabbb
bbbabaabbbbaababaaababaa
aabaabbbbaabaabbbabaaaaaabbbbbbb
aaabaaaabaaabbaabbaabbbaabababab
abbaaabaabaaaabaabbbbbab
aaaaabaabaaabbaaababaabaaaaaabbabbaaabbbaaaaaababbbbaabbbaabaabababbbaababababab
bbbbaaaabbbaabaaaabaaabbababbaaaaaaabbbababaaaabbbbaaaab
bbbabaabbbaaaaabbabbaaab
abbaaaabaaaaaabaaabbbbaaabbbbaababaabbba
baabbaaaaaabbaaabaababbb
aaabaabbbabbaabababaabbbaabbaababaabaaababaaabbb
bbabaababbbabbabaabbaaab
bbaaaaabbbaabaaaaaabbaaa
bbbbbaaabbbabaabbaababbabaaaabbaabbababa
aabbbbaabbaaabaababbbbbababbbbbaabbaabbbbaaaabbbababbaab
aabbbabbaaabbaaabaababbaabaaaaaaababaaabaaabbbbaaabaaabaaaabbaabababaaaabaaabababbbaaaab
aaabaaaabbbbabaaababbbbbabbaaaaa
abaabbbbabbaaabaaaaaabaabbbbbbaa
bbaaabaaaaaabbabaabbbbab
baaaaaabaaababbabbbbaaab
bbabaabbbabbbaaaaabbabbaaabbaababbabaabbaaabaaaaaaabaaba
abbbbbaabaabbbabbabbaaaabaaaaaba
aaabbaaabbbbaaaababaabbbbbbabbbaaabaaabbbaabbabaaabbaaab
babbaababbbbaaaabbbbaabaaabbbbaaabbabbba
baababbabbaaaaabbbababaabbabbaaa
aabbababbbaaaabaabbabaaabbbbbaaabaaabbbaabbbabab
abbbaabaabaabaababababbaabbbbaabbbabbaabbaabbaaa
baaabaaabaaabaaabaaaaaba
bbbabaabaabbbabbbbabbaaa
bbaaabaabbbbaaaabbabbabaabbababb
abababbbabaaabbabaaaabbbaaaabbabbaababbabbaabbbb
bbbbbaababbaaaabbaaabaababbbaaab
baabbabbbbaaaababaabbaaabaababaabaaaabab
baababbaabbbbbbaaabbabbabbbbababbabbbbbbaabbaabb
baabaaaabaaabbbaabababbbaaabbaba
abaabbabbaabbaaaaaabaaababbaabaabbbaaaaa
baabaaaaabbaabbbabaabbba
bbbaabbababbaabaaaaaaabb
bbbbabaababaabbbbbababaabaaabaabaaaabbaa
aabaababbaabbbbababbaabababaabaabaabbaaaabababab
abaaaaabbbbaabaabaabbaba
bbbbaabbbaabaaabaabbaababaabbabbbabaaaaaaaabaaba
aabbabbaabaaaaaabbabaababbbaabaababaaabb
abaaaababbbaabaaaabaaaaabaabaaaabaaabaaaaaaabaab
aaaaaabaaabbbababbbbbababbbbababaabbbaab
bbbabbababbaabbbabababaa
baabbbaaaababbbabbbaaaba
baaaaaabaabbbabbbaaaaaabababaaba
bbbbaaaaabbbabbabbbbaabaaaaaababbbaabbbb
aaabababbaabbaaaaabbbbab
abbbbaaabbaabbababaaabbabaaaabba
bbaaaababaaabbaaabbbaaaa
ababbbbbaaaaaabaabbabbbbaaabbbbbabababaa
bbabaabaabbabaaabbbaabbb
aababbbaabbbabbabaaabbbbbbabaabaabbbbaaaabaabbaabababaab
aababbabbabbabbaabaaaaaabbbababbabbabbbbabbaabba
aabbbbbababaaababbaaabaababababbbabbaaabaaabbbaabbbabbab
ababbbabbabaaabaabaabaaababaaaaa
abbbbbbabbbaabbaabbabaaabbbbabaaabbbbbaa
bbaaaaabaabbaabaaabbabaa
bbbbaabbabbbaababbaababa
babababbababbbbabbabbaaa
aabaabbbaaabbbaabbbbabaaabbababa
abbbbbabaaabbaaaaabbbabaabbbbaaabbbaaabbbaaababa
baabbaaaaaabaabbbaabbbaaaaabbbaaaabbaababaaaabbbaaaabbba
abaaabbaababbaaababbaabaaaaaabababaabaab
bbbabbabaababbbabaababaa
bbaabbabbbbaabbabbbbaaab
abbbbabababbbabbbbbbabbbbbabaaaaabbaaabb
aabaabbbbbbbaabbabbabbbbaabaabbbbaabbaaaabaabbabaaaababbabbabbab
aabaaaaabaaabaabbbabbaabaababbabbbaababababbbbab
baabaabaabbaababababaaba
aabaaaaaabbbbbabbaabaabb
aabaaabbaaabbbbaaaaaaaaabaaaaaba
babbbabbbbbbabbaaaabbaaabababbbbabbbaabbbaababbabbaabbbbabbbbbaabaaaabba
baabaababaaabbbbabbaabba
bbababaabbaabbbaabbbaabbababaaabbababbaaabbbaaabbbaabbbb
abaaabbabbaaabbaaabaabaa
babababbabbabbbbbababbaaaaabaabbaabaabbbabaaabababababaabaababab
bbabaabbaababaabbbaabaaaabbbbaabaaaaaabb
aababbaaabababbbbababbbbbbbbabbaabbaaaaabaababaa
abaabbabbaaabbaabaaabbba
bbbbaabaaababbababbbbbaa
baaabbbbbbbbbaaabaabbaaabbaabaab
aabbbababbababaaabbbaabbbbabaaab
bbbaabaaabbbabbaabbbbaaaabbbaabbabababbaabbbababbbaabbbb
aababbbabbabaababaaababbaabbbbaabbaabababbaaaabbaababaaa
bbaababbbabaabaaaabbbbbb
bbbabbbbabbabaaaababbbbabbbabaababaabbbbaababbbbabbbaaababbbabaabbabbbaa
bababbbbaabbaaaabbabbbab
bbabbabaaabaabbbaabbabaa
baabaaaaababbaaabbbbbabb
bababababbbbabbaabbaaaabaaabbabb
baaaaaababaaaaabbbaaabbb
ababbbbababababaabbabbab
baabbbaaabbabbbbbbaaaabb
bbbabaabbbaabbbabbabbbbb
aabaababaaaabbabbabbabaa
bbbbbabaabbbaaaabbaababaabaaababbbaabbabaababaaabbbaabaa
abbbaababababbbbabababba
bababbabbbbbabbabbbaababbaaabbabbabaabbaaabbbaabababbbaa
baabbbaabbbabbbbbbaaabab
abbbaabbaabaaabbbbbbababbbbbaabbbabaaabbabbbaaababbabbab
bbbbaaaaababbbbbaabbabbabbbbbaababbababbbabbaaaabbabaaab
bbbabbbbbaabaaababbbbaaaabbbabbbabbbabab
abbbaababbbbbbbbaaaabbabbaaabbbb
bbbababbaabbbabaabbbabab
ababbaaabbaaaaabaaaababa
bbbbabbaabaabbbabbabbbbbbbbbabbbaabbbababaaaabbaaaaaaabbbbaaaaaa
bbabaabbabababbbaaabbbaabaaabbaaabababbbabbbaaaaaaaabbaa
aaaaaaaaaaabbbbaaaabbabbaaaaabba
"""
