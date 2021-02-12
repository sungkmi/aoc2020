package sungkmi.aoc2020.day10

def parse(inputString: String): List[Int] =
  inputString.split("\n").map(BigInt(_).toInt).toList

def countJolts(joltageRatings: List[Int]): (Int, Int, Int) =
  val sorted = 0 :: joltageRatings.sorted
  sorted.zip(sorted.tail).foldLeft((0, 0, 1)):
    case ((j1, j2, j3), (i1, i2)) =>
      (i2 - i1) match
        case 1 => (j1 + 1, j2, j3)
        case 2 => (j1, j2 + 1, j3)
        case 3 => (j1, j2, j3 + 1)
        case _ => throw new Exception("Wrong joltage diff")

def countWays(joltageRatings: List[Int]): BigInt =
  (0 :: joltageRatings.sorted).foldRight(List.empty[(Int, BigInt)]):
    case (n, Nil) => (n, BigInt(1)) :: Nil
    case (n, xs) => (n, xs.takeWhile(_._1 <= (n + 3)).map(_._2).sum) :: xs
  .head._2

@main def part1: Unit =
  val (j1, j2, j3) = countJolts(joltageRatings)
  val ans = j1 * j3
  println(ans)

@main def part2: Unit =
  val ans = countWays(joltageRatings)
  println(ans)

lazy val joltageRatings: List[Int] = parse(input)

lazy val input: String = """160
34
123
159
148
93
165
56
179
103
171
44
110
170
147
98
25
37
137
71
5
6
121
28
19
134
18
7
66
90
88
181
89
41
156
46
8
61
124
9
161
72
13
172
111
59
105
51
109
27
152
117
52
68
95
164
116
75
78
180
81
47
104
12
133
175
16
149
135
99
112
38
67
53
153
2
136
113
17
145
106
31
45
169
146
168
26
36
118
62
65
142
130
1
140
84
94
141
122
22
48
102
60
178
127
73
74
87
182
35"""
