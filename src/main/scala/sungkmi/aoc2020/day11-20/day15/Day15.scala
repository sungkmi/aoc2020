package sungkmi.aoc2020.day15

case class State(
  lastSpokenTurn: IndexedSeq[Int],
  lastSpokenNumber: Int,
  turn: Int,
)

extension (s: State)
  def next: State = State(
    lastSpokenTurn = s.lastSpokenTurn.updated(s.lastSpokenNumber, s.turn),
    lastSpokenNumber = s.lastSpokenTurn(s.lastSpokenNumber) match
      case 0 => 0
      case n => s.turn - n,
    turn = s.turn + 1,
  )

object State:
  def withStartingNumbers(startingNumbers: Seq[Int], size: Int): State = State(
    lastSpokenTurn = startingNumbers.init.zipWithIndex
      .foldLeft(Vector.fill(size)(0)):
        case (memory, (n, i)) => memory.updated(n, i + 1)
    ,
    lastSpokenNumber = startingNumbers.last,
    turn = startingNumbers.size,
  )

def parse(s: String, size: Int): State =
  State.withStartingNumbers(s.split(",").toSeq.map(_.toInt), size)

def solve(input: String, until: Int): Int =
  @annotation.tailrec
  def loop(state: State): State =
    if state.turn >= until then state else loop(state.next)
  loop(parse(input, until)).lastSpokenNumber

def solve1(input: String): Int = solve(input, 2020)

def solve2(input: String): Int = solve(input, 30000000)

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit = 
  val ans = solve2(input)
  println(ans)

lazy val input: String = "9,19,1,6,0,5,4"
