package sungkmi.aoc2020.day22

import scala.collection.immutable.Queue

case class Decks(p1: Queue[Int], p2: Queue[Int])

extension (d: Decks)
  def step: Either[Boolean, Decks] =
    //println(s"===> $d")
    dealWithTopCards:
      (p1h, p2h) => buildNextDeck(p1h > p2h, p1h, p2h)
  @annotation.tailrec
  def combat: Queue[Int] = d.step match
    case Left(true) => d.p1
    case Left(false) => d.p2
    case Right(d1) => d1.combat

  def rStep(depth: Int, history: Set[Decks]): Either[Boolean, (Set[Decks], Decks)] =
    //println(s"${"  " * depth}$d")
    if history contains d then Left(true) else dealWithTopCards:
      (p1h, p2h) =>
        val isP1Win: Boolean =
          if d.p1.size > p1h && d.p2.size > p2h then
            val rd = Decks(d.p1.tail.take(p1h), d.p2.tail.take(p2h))
            rd.recursiveCombat(depth + 1, Set.empty)._1
          else
            p1h > p2h
        (history + d, buildNextDeck(isP1Win, p1h, p2h))
  @annotation.tailrec
  def recursiveCombat(depth: Int, history: Set[Decks]): (Boolean, Decks) =
    d.rStep(depth, history) match
      case Left(isP1Win) => (isP1Win, d)
      case Right((history1, d1)) => d1.recursiveCombat(depth, history1)

  def dealWithTopCards[A](whenBothAvailable: (Int, Int) => A): Either[Boolean, A] =
    (d.p1.headOption, d.p2.headOption) match
      case (None, None) => throw new Exception(s"Empty decks")
      case (Some(_), None) => Left(true)
      case (None, Some(_)) => Left(false)
      case (Some(p1h), Some(p2h)) => Right(whenBothAvailable(p1h, p2h))

  def buildNextDeck(isP1Win: Boolean, p1h: Int, p2h: Int): Decks = isP1Win match
    case true  => Decks(d.p1.tail :+ p1h:+ p2h, d.p2.tail)
    case false => Decks(d.p1.tail, d.p2.tail :+ p2h :+ p1h)

def parse(s: String): Decks =
  val Array(p1, p2) = s.split("\n\n").map:
    deckString =>
      Queue.empty[Int] ++ deckString.split("\n").tail.map(_.toInt)
  Decks(p1, p2)

def evalDeck(deck: Queue[Int]): BigInt = deck.reverse.zipWithIndex.foldLeft(BigInt(0)):
  case (acc, (card, index)) => acc + card * (index + 1)

def solve1(s: String): BigInt = evalDeck(parse(s).combat)

def solve2(s: String): BigInt =
  val (isP1Win, Decks(p1, p2)) = parse(s).recursiveCombat(0, Set.empty)
  evalDeck(if isP1Win then p1 else p2)

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit = 
  val ans = solve2(input)
  println(ans)

lazy val input: String = """Player 1:
42
29
12
40
47
26
11
39
41
13
8
50
44
33
5
27
10
25
17
1
28
22
6
32
35

Player 2:
19
34
38
21
43
14
23
46
16
3
36
31
37
45
30
15
49
48
24
9
2
18
4
7
20
"""
