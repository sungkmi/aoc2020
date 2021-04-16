package sungkmi.aoc2020.day17

type Coordinate = (Int, Int, Int)
type State = Set[Coordinate]

extension (s: State)
  def neighbors(p: Coordinate): Set[Coordinate] =
    val (x0, y0, z0) = p
    val ps = for {
      i <- -1 to 1
      j <- -1 to 1
      k <- -1 to 1 if i !=0 || j !=0 || k !=0
      (x, y, z) = (x0 + i, y0 + j, z0 + k) if s `contains` ((x, y, z))
    } yield (x, y, z)
    ps.toSet
  def range: (Coordinate, Coordinate) =
    s.tail.foldLeft((s.head, s.head)):
      case (((x0, y0, z0), (x1, y1, z1)), (x, y, z)) =>
        ((x0 min x, y0 min y, z0 min z), (x1 max x, y1 max y, z1 max z))
  def step: State =
    val ((x0, y0, z0), (x1, y1, z1)) = range
    val s1 = (for {
      x <- x0 - 1 to x1 + 1
      y <- y0 - 1 to y1 + 1
      z <- z0 - 1 to z1 + 1
      p = (x, y, z)
      neighborSize = neighbors(p).size
      if (s.contains(p) && (neighborSize == 2 || neighborSize == 3))||
        (!s.contains(p) && neighborSize == 3)
    } yield p).toSet
    val p0 = s1.range._1
    s1.map:
      (x, y, z) => (x - p0._1, y - p0._2, z)
  
def parse(s: String): State =
  val lines = s `split` "\n"
  lines.zipWithIndex.toSet.flatMap:
    (line, x) =>
      line.zipWithIndex.toSet.flatMap:
        case ('#', y) => Set((x, y, 0))
        case _ => Set.empty

def solve1(input: String): Int =
  val state = parse(input)
  (0 until 6).foldLeft(state):
    (state, _) => state.step
  .size

type Coord4 = (Int, Int, Int, Int)
type State2 = Set[Coord4]

extension (s: State2)
  def neighbors(p: Coord4): Set[Coord4] =
    val (x0, y0, z0, w0) = p
    val ps = for {
      i <- -1 to 1
      j <- -1 to 1
      k <- -1 to 1
      l <- -1 to 1 if i !=0 || j !=0 || k !=0 || l != 0
      (x, y, z, w) = (x0 + i, y0 + j, z0 + k, w0 + l) if s `contains` ((x, y, z, w))
    } yield (x, y, z, w)
    ps.toSet
  def range2: (Coord4, Coord4) =
    s.tail.foldLeft((s.head, s.head)):
      case (((x0, y0, z0, w0), (x1, y1, z1, w1)), (x, y, z, w)) =>
        ((x0 min x, y0 min y, z0 min z, w0 min w), (x1 max x, y1 max y, z1 max z, w1 max w))
  def step2: State2 =
    val ((x0, y0, z0, w0), (x1, y1, z1, w1)) = range2
    val s1 = (for {
      x <- x0 - 1 to x1 + 1
      y <- y0 - 1 to y1 + 1
      z <- z0 - 1 to z1 + 1
      w <- w0 - 1 to w1 + 1
      p = (x, y, z, w)
      neighborSize = neighbors(p).size
      if (s.contains(p) && (neighborSize == 2 || neighborSize == 3))||
        (!s.contains(p) && neighborSize == 3)
    } yield p).toSet
    val p0 = s1.range2._1
    s1.map:
      (x, y, z, w) => (x - p0._1, y - p0._2, z, w)
  
def parse2(s: String): State2 =
  val lines = s `split` "\n"
  lines.zipWithIndex.toSet.flatMap:
    (line, x) =>
      line.zipWithIndex.toSet.flatMap:
        case ('#', y) => Set((x, y, 0, 0))
        case _ => Set.empty

def solve2(input: String): Int =
  val state = parse2(input)
  (0 until 6).foldLeft(state):
    (state, _) => state.step2
  .size

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit = 
  val ans = solve2(input)
  println(ans)

lazy val input: String = """.##..#.#
##.#...#
##.#.##.
..#..###
####.#..
...##..#
#.#####.
#.#.##.#"""