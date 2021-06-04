package sungkmi.aoc2020.day23

case class CupCircle(
  cups: Vector[Int],
  pickupMap: Map[Int, Vector[Int]],
  size: Int,
) {
  override def equals(that: Any): Boolean = that match
    case thatc: CupCircle => this.toList == thatc.toList
    case _ => false
}

object CupCircle:
  def apply(cups: Vector[Int], current: Int): CupCircle =
    val (f, e) = cups.splitAt(cups.indexOf(current))
    CupCircle(e ++ f, Map.empty, cups.size)

  def parse(s: String): CupCircle =
    val cups: Vector[Int] = s.map(_ - '0').toVector
    CupCircle(cups, Map.empty, s.size)

extension (c: CupCircle)
  def toList: List[Int] = pickupN(c.size)._2.reverse

  def pickupThree: (CupCircle, Vector[Int]) =
    val (c1, acc) = pickupN(3)
    (c1, acc.reverse.toVector)

  def pickupN(n: Int): (CupCircle, List[Int]) = (1 to n).foldLeft((c, List.empty[Int])){
    case ((cupCircle, acc), _) =>
      val (c1, p1) = cupCircle.pickupOne
      (c1, p1 :: acc)
  }

  def pickupOne: (CupCircle, Int) = 
    val cupCircle1 = c.pickupMap.get(c.cups.head) match
      case None => c.copy(cups = c.cups.tail)
      case Some(pickups) => CupCircle(pickups ++ c.cups.tail, c.pickupMap - c.cups.head, c.size)
    (cupCircle1, c.cups.head)

  def move: CupCircle =
    val (c0, current) = c.pickupOne
    val (c1, ps) = c0.copy(cups = c0.cups :+ current).pickupThree
    @annotation.tailrec
    def validDestination(current: Int): Int =
      if ps contains current then validDestination(current - 1)
      else if current < 1 then validDestination(c.size)
      else current
    val dest = validDestination(current - 1)
    //println(s"===> $ps / $cups0 / $pickupMap0 / $dest")
    val ans = c1.copy(pickupMap = c1.pickupMap + (dest -> ps))
    //println(s"===> $ans")
    ans

def solve1(s: String): String =
  val cups0 = (0 until 100).foldLeft(CupCircle.parse(s)){
    (cups, i) =>
      //println(s"===> #$i: $cups")
      cups.move
  }
  //println(s"===> solve1: $cups0")
  val cups1 = cups0.toList
  val indexOf1 = cups1.indexOf(1)
  (cups1 ++ cups1).drop(indexOf1 + 1).take(cups1.size - 1).mkString

def solve2(s: String): BigInt =
  val cupsFront: Vector[Int] = s.map(_ - '0').map(_.toInt).toVector
  val cups = CupCircle(cups = cupsFront ++ ((cupsFront.size + 1) to 1000000), Map.empty, 1000000)
  val cups1: List[Int] = (1 to 10000000).foldLeft(cups){
    (cups, i) =>   
    //if (i % 500000 == 0) then println(s"===> solve 2 perform #$i")    
    cups.move
  }.toList
  val indexOf1 = cups1.indexOf(1)
  val List(n1, n2) = (cups1 ++ cups1).drop(indexOf1 + 1).take(2)
  println(s"===> n1: $n1, n2: $n2")
  BigInt(n1) * BigInt(n2)

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit = 
  val ans = solve2(input)
  println(ans)

lazy val input: String = """487912365"""
