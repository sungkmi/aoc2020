package sungkmi.aoc2020.day24

class Day24Test extends munit.FunSuite {

  val testInput = """sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"""

  test("day24 parseLine") {
    assertEquals(parseLine("esew"), D(1, -1))
    assertEquals(parseLine("nwwswee"), D.reference)
  }

  test("day24 parse") {
    val flipCountMap = parse(testInput)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toSeq
      .groupBy(_._2)
      .view
      .mapValues(_.size)
      .toMap

    assertEquals(flipCountMap, Map(1 -> 10, 2 -> 5))
  }

  test("day24 solve1") {
    assertEquals(solve1(testInput), 10)
  }

  test("day24 day") {
    val initialBlackTiles: Set[D] = findBlackTiles(testInput).toSet
    
    assertEquals(day(1, initialBlackTiles).size, 15)
    assertEquals(day(2, initialBlackTiles).size, 12)
    assertEquals(day(3, initialBlackTiles).size, 25)
    assertEquals(day(4, initialBlackTiles).size, 14)
    assertEquals(day(5, initialBlackTiles).size, 23)
    assertEquals(day(6, initialBlackTiles).size, 28)
    assertEquals(day(7, initialBlackTiles).size, 41)
    assertEquals(day(8, initialBlackTiles).size, 37)
    assertEquals(day(9, initialBlackTiles).size, 49)
    assertEquals(day(10, initialBlackTiles).size, 37)

    assertEquals(day(20, initialBlackTiles).size, 132)
    assertEquals(day(30, initialBlackTiles).size, 259)
    assertEquals(day(40, initialBlackTiles).size, 406)
    assertEquals(day(50, initialBlackTiles).size, 566)
    assertEquals(day(60, initialBlackTiles).size, 788)
    assertEquals(day(70, initialBlackTiles).size, 1106)
    assertEquals(day(80, initialBlackTiles).size, 1373)
    assertEquals(day(90, initialBlackTiles).size, 1844)
    assertEquals(day(100, initialBlackTiles).size, 2208)
  }

  test("day24 solve2") {
    assertEquals(solve2(testInput), 2208)
  }
}
