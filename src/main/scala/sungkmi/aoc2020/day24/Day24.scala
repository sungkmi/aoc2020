package sungkmi.aoc2020.day24

// x: 3 o'clock, y: 1 o'clock
case class D(dx: Int, dy: Int)

object D:
  val reference: D = D(0, 0)

extension (d: D)
  @annotation.targetName("plus")
  def +(other: D): D = D(d.dx + other.dx, d.dy + other.dy)

enum Direction(val d: D):
  case E  extends Direction(D( 1,  0))
  case SE extends Direction(D( 1, -1))
  case SW extends Direction(D( 0, -1))
  case W  extends Direction(D(-1,  0))
  case NW extends Direction(D(-1,  1))
  case NE extends Direction(D( 0,  1))

def parseLine(line: String): D =
  @annotation.tailrec
  def loop(xs: List[Char], sum: D): D = xs match
    case Nil => sum
    case 'e'        :: tail => loop(tail, Direction.E.d  + sum)
    case 's' :: 'e' :: tail => loop(tail, Direction.SE.d + sum)
    case 's' :: 'w' :: tail => loop(tail, Direction.SW.d + sum)
    case 'w'        :: tail => loop(tail, Direction.W.d  + sum)
    case 'n' :: 'w' :: tail => loop(tail, Direction.NW.d + sum)
    case 'n' :: 'e' :: tail => loop(tail, Direction.NE.d + sum)
    case _ => throw new Exception(s"Wrong input: ${xs.mkString}")
  loop(line.toList, D.reference)

def parse(s: String): Seq[D] = s.split("\n").map(parseLine).toSeq

def findBlackTiles(s: String): Seq[D] = for
  (tile, tiles) <- parse(s).groupBy(identity).toSeq
  size = tiles.size if size % 2 != 0
yield tile

def solve1(s: String): Int = findBlackTiles(s).size

def step(blackTiles: Set[D]): Set[D] =

  val blackNeighbors: Seq[D] = for
    blackTile <- blackTiles.toSeq
    neighborD <- Direction.values.toSeq
  yield blackTile + neighborD.d
  
  val blackNeighborCountMap: Map[D, Int] = blackNeighbors
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toMap
    .withDefaultValue(0)

  val becomeWhites = blackTiles.filter{ t =>
    val neighborCount = blackNeighborCountMap(t)
    neighborCount == 0 || neighborCount > 2
  }
  val becomeBlacks = for
    (tile, neighborCount) <- blackNeighborCountMap
    if neighborCount == 2 && !blackTiles.contains(tile)
  yield tile

  blackTiles -- becomeWhites ++ becomeBlacks

def day(n: Int, initialBlackTiles: Set[D]): Set[D] =
  (1 to n).foldLeft(initialBlackTiles)((blackTiles, _) => step(blackTiles))

def solve2(s: String): Int = day(100, findBlackTiles(s).toSet).size

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit = 
  val ans = solve2(input)
  println(ans)

lazy val input: String = """wswwwwwwwnenewswnewswwswswww
wsenenwswnwwnenwswwsenwwwnenwwnww
eenwenewwswsweeneswswseeeeneee
nweenesenwnenenwnenenwnwnwnwnwnwswswnwnw
swwnwwwwwwewnwewwswswswswe
nwnenwnenenenwnwneenenenwnwswswnwnenwesenw
seneswwswseswseseseseenwsenesesenewnw
nwnwwnwnwnwenwnwnenwnwnwnw
swnwnenweswwswnwsenwnewnweswseneeesesw
eenesewnwswwnweswneswseenwswsw
neeseswsesesweseseeseseeeseeenwe
wneswnenweseseswwwwesenwnwnenwwe
senenwneneneswneneneenwneneeeneenene
neneswwseneneneneeeneneneenenenene
eeseeenwnwseeneneswneeeeewewe
eeeeeesweeseeswnweeeeeenwse
nwnwnwnwneeswneswswwwsewswnenwnwwwnw
nwswneswswsenewnewswwswwswswwswswesesw
eneseneeneswwwneeseneneenweneneee
neneneenenenwenewnenwnwnenesenewnene
wnwwnwnwnwwwnwnwnwswenwnewnwenwnwnw
nwwseneenenenwneswnenenw
seeseneneeneseewneenewnwsenwwnee
seswseswseseseseswseswseseswsenwsenwseswnw
eeweswneeeeneeswnwnweneeenesenee
swswswswswswnwwswseswnwswwseswne
eneeeeesenesewnwwenweseenenee
nenwnenwseewnenwnenenenenene
seswseseneseswnwswseeenwseswwswsesesese
nwenwsenwwsenwsenwnwnwnewnwnw
nwnweswnwnwnwnwnwnwnwnwnwwnwnwsenwwnwe
wneneeneneeeeeeeeeeswwesenee
nwnwewswnwnwsenewnweseseenesenewnenww
swswswswswswswswswwsweeenweswwnwswnwsw
neneswswwweswswsewwwswwswwswswswnw
wneswwwseenwewnwwnwswwwswwswwsew
nwenwswnwnwneswnenenwsewswnwnwnwsenwnwnw
eseswsesesesenwseenwseseee
swseswswsweseswswswwneswseswswnwswsesewse
nenenenwwneenenenenenenenenenwne
nenenwnenwnenenesenenewnwnenw
nenwswseswseeswswswswseseww
swswwswswswwswswswswswswswsewswnewe
weneswnenwnwsenenenewnenenweswwnenenw
neweswseneseswswswswswswsww
wsewsewswwnwnewnwnwswswewswwwwsew
wneseswneswenwwewnenwneneneneesenw
nwnwneeeswnwnenwwswnwnwnenwnwenwnwewsw
swneseseenewneneneewnenesenewesewnw
sesesesenwsenwseseswwseseneeeenwsewe
seenwesweeeesweeseenwese
wwnwnwnewwenwwnwswwwwnwnwseww
esweswswswneenenwnenesw
wswwwwswwsweswwwwwwwnewnwsw
eeeseeeesenwseswseeeeseenweesw
enwsewnweeeeenwseswenwenweseswe
wswswswswwswseswneswswwneswswswwswne
seneseneseswwsesesenesewswswsesesesese
eeseeseeneenweseneswneewswwsese
swswsewwneswswwnww
seseseeseneesesewsesesewseseseseese
seseneseswseswsesewseseseswsese
weeswnwseseswenewseenenenwewwesw
nwswswswswsesweswswswswswseweswswswsw
swseswsenwnwseseseeesesenwnwseseseesese
nwwswwswswswswswnwwwwseswseseneswsw
enwswnwseeeeeenwswnwnwseneswewsw
wwswswwswwwswsenwwwswswswsw
swswsewnwnwseswwswswwnewwwswwseww
wwnwnenwwwsenwnwwewwsewwww
swnwsenenwnenwnesewneneneneneswsw
nwnwwwwneswsenenwnwnwsewwwnwenwnww
seseseseeeewseeenese
seseswnwwseseseswswseseseseneseseseneswse
nwnenwnwnenwnenenewenenwwsenwnenwnese
nwnwwnwewnewnwnweenwswnwnw
nwnwsewwnwnwsenwwwwwnwwnw
eeeeneneeeenew
wwswwwswwnwneswse
wswswswswwswnewwwneswwwwseswwsw
eseesenwsenweseeesewsesenwseesesw
eseswenweeenwswsenweseneeswweeene
nwnwnenesenwwenwnwsweneneenwswsenwnwswe
swsenwsenwenewnewsesewwnwenwwsenwnw
wswwseswsenwswswsweseswsesweswswseeswse
eneswnenwwswneswneneneneeeneenenenene
enwnwnwnwwnwnwwnwenwnwnwnwwsenwnwnwnw
nwnwsewnwnwnwnwnwnwnwnwnenwnwnw
nenesewwswswneewswswneneseswsenwseswsw
nwnwnwswnwenesenwnwnenwnwwnwnwnwnwneeww
swnwnwnenenwenwnwnenenwnenwnenwnwswnenw
nenenenenenenwnenwneneneseswsw
nwenwneswnenenenwnwnwnw
swneeswswnwsewnwwswswswswswswwwsesw
wwenwnwwsenwwwwnenwswwnwwnwnenww
sweswswswwswswswwswwewnwswwsww
nenwnwnwnwnenwswnenenwnwnwnwnw
nwnenwnesenewneenwnwsenenwnwswwnesenwne
nenwwwwwwwwswwwwnw
nwsenwswseseseseseseeswnewseswseesesesese
newwneneneseeneeeeeeneeswenenw
sweeeeeeeeenwsweeneneeenwesw
swwwnenwenenwswnwswseesesenwwsewnese
seswwswswnwseseswseseseeneseswnenweswwse
nwnwesenenwnenwsesweswnwnwnesenwsww
sesesewneenenwnwnwneeenwenesenee
nwswneenwnwsenesenwwnenwnwnenewnwnwnenwnw
wneneswnwneeneeneneneeweneeeeene
wwewnwwwwwwnw
wnenwnwsewwnesenwnwnwsewswenwwwse
swwswsesenwswswseeswseswswswnenwsw
seewwsesewseswseneseswswseswseseswnwe
nenwnwswweweseesewesewseeeeee
sweeneseesweeneesenwsweewneeese
nwneeeswswwnenenenenesenwnwnenwnwnene
wwwswsewwwwswewwneswwwwswsw
nwnwnewsweneneseswnwnwnwnwnenw
sweseneswwswseseseswseswseswseswnwsese
sweswswnwswnwseswwswnwswewsweswneswsw
swneswwwsweswswneseswwwwswnewwwwsw
esweewweweeeeeeneeeeeee
wwnwnwswenwnwnenwnenwsenwnwnwnwsenwnw
nwnwnwnwnwenenwnwnwwnwnwnwswnwnwnwe
wnwwwweweswwswnwswwwwswesww
seseseseseseeseeeeeneseswnwewse
seswswswswwsewswene
nwwwwnewswwwnww
eseeneseeenwswnenesweeenweenwnwene
newwnwwnwswsewswswnwnenwwwnewwenw
seeeweseswwswsenwnenweeenwsesewne
newwseeeswneeene
neneneneneneneneneswnenenenenewnenenese
wnwswenwswnwnwnwnwnenwnwnwnw
swseseesenwwseswseweswseswnwsesweswsw
sesenwseseseseeweeseesesesweseese
sewswnesenwseswnwseseenese
wwnewewswseswenweewwwnew
wswnenenwswswswswnesenwseneseswnwswswse
nwsesenenenweseneneseswwwnewwswewsenw
swnewwneswewwwswweenwswwnwwnw
nenwswnwseseswswsweswswnewwsenwswesesese
newseeneneneenewneswnesewnwenwneeswne
nwswwesewwwneswswswwwwwswwwew
wwewwwwwswnewwwnwewwwwww
eseeeeeeneseeeewseseneswseesew
nwnweeswseneenenwwseesewewsweee
seewwwswnwnwswseswweswwwneswwswnw
nwnwwswenwswnwenweswnwswnenwnenwnwne
wswswswwwnewswwwwwseewswwwsw
enwweneeneneneneeenewnesweseneee
senewnesweneneeeeeeeeewnwenese
neeswneseeeeeswneneneswenwwww
eeseseeswseeeseneeeeesewenee
senwseneseseseseseeswsenwsesesesesesw
swswswnwswswsesweswswswswenwnw
eswswswswswwwswswsweseswswswnesenwsw
wwnwwnwnwwneneesewnwwwnwwsesww
swswswswswswweswseswswwswnew
neeeneeeeseeeeeswnwsweeesenwne
wnenwnwneswnwnewweswnwnwseswnwnwnwe
nwnwnwneswswenwenenwnenenenenwnwnenenenw
senenenwsesesewseswnenwswseswweesesw
swswneseseswnwsenwseswseseswswseswswsese
neenwswseswneeneneneneeenenwneswneenene
nwnenenwnesewnwnwswnwnwnenwnwnwneenenwnw
swsewsewwwwwwnenww
nwneswnenwnwnewnenwnwnwenwnwswnwneenwse
seseeweneswnwwwsenwsweeeenenwnww
eswwwwweswwsewnwnwnwenwswwwwnww
wwneeswwnwwwwwswswewwwwwsesw
wwswneswweswnwsweewsenenw
nwnesenwswnwswseweswseseswwswnww
eneeneenewseeneee
sweeeneeeeeneneeeweneeeeesw
wnewnwnwwsenwnwwnwewwwwnwnwnww
eeewnweeeseswee
nenwswseseneseswnwseeseswseneswnenwnwsesw
wswwswwwwwwwwnwnewnwnwenwnww
wswswnwswsweswwsww
eswswswwseweneswseswwneseswswswsesesw
swswwwnwnesenwsesesw
swseswswnwswwswswswswnweswweneswneswnw
sewseswsewseeseseseseseseswsesesesene
wesewwwwswwnwwwwenewwwww
neenwneneswnenenenenenene
nwnenwnwnenenenesweswneneneswnenenenese
eneenwseeeseswsesesweeeeseeese
swswnwesenwsenweseenwsesenwswenwneswsw
swswswsweswswswswnesenwswnwswswswswesw
seseswnwsweswswseswswswswswswsw
seseewnenwswnwsesenenweswse
wwnwwnwwwwsenwwnwswwwwnenwwne
newwsenwnwnwswnwwnwsewne
eswneswnewneseeeeeeeneeeneene
wenweswnesenwwneneenwneswnenewnenwse
nesenenwwneneewnenwnwnenwnwnenwenwnwne
sewswswesewnwswnewnewswwneswnwsesw
eseswswseseseseseseswswnwswnwseeswswwsesw
nenesesenwnesenenwswneneswewnwsenweewsw
eeeseseseeneesesw
nwsenwneseneeewe
nwwneswwnwnwwwwwesenwnwnenwnw
eseeseeseneseseseseseseswse
nwswswswwnwswseswswswnwsesweneseseewnw
nenwneeswewswswnwseneswseneeesenwwnwne
swneseeswnwnwsenwwwewnwwswnewwwew
swnenesenenenwnenenewnenenwwneneneeneene
swnwswswwnewnwswwswswswswswswswsweseew
seseseenwswswseneswsewnesewswseswnee
nwnwnenwsenwnenwseesenewnwnwnwwwwswse
nesenenesewwwsenwnenwnwnewnesenesesenesw
wnwneneseswswnwnenenwnenenwnwnwnwsenenw
nwwswwnwnwnwsewwwwenwnwnwne
wwwnwseneneeswnwnwneswwseswnwnwnenese
esesesenwseseeseseeneswseseesee
nwnenwnwnwneseneneneswwnwwnwsenenwnwnwne
seeeewseeseeneswenwneeewnenee
seswneesenewswwnwswseseswsesesesese
neswseseseseewsesesesesesesenwsesenew
sweswseseneseseseseseesenwsewsesewsw
neweseneneeenwsweeneneneenenenee
nenwnesesweenenenewsweneswneneeneseee
nenwwseswwseseneseneseseseseewswese
wwnwnewnwnewnwwwsewnwwwswnw
nwwwwwnewnwewsewnwnwwnwwnwwnw
wnewwwwwwnwwwwwnwwsewwnese
neeesenewswwenwseswnesweneenewnwnw
swsenewnwnwswneeneneneneesenwwswnenee
weenwswseeweeeenwseseseseeswnw
nenwsenesweeneneeneeenwswneeeene
eseswwenesweeneeenwwenwnenwseene
eeneneeseeseenwswenwsewenwnw
swnenweseneneeeneswnwseneenwswswenesw
swsesewwwswnewnewswswswswwwswswsw
wneweeswseewseneseewswnewnwswswse
wwsenenwnwwswnwwwnwwnwwwnwwnwnwe
neenwneneneneneneneseene
wseswnewneswwwnewnwwwenwwwnwsew
esweswwsewseswnewsesweswswswswnwsesw
swseswswswswswsenweswneswswse
nwwwwswnwswnwwwwwnwnenwenwwwse
nwenwswnenwnwnwnwswnwwnwnwnwnwenwnwnw
nwnwnwsenwnwnwnwnwnwwnwnwnwnwnwnwe
swswswswswswswswswswwswswseswneswnwsw
seseswswswseseseneseswwseseseneswwnese
nweeenwsesesweenwnwsweeeweswe
nwesewwswnewewnewnwsese
wnwnewwwsewsenwnwnwsewwnwwwnwww
wswswnwwwwswswwswwwewewswnww
nwnwnwenwneeswnwnenwwnwnwsenewnwnwnwne
nwewwswnwnwnwswenenwwseseenwseesww
nwnenewneeeneneeseweneeneeswnene
neneneeeswnenwnewneneneeswseneenwse
nwnwwwesenwwwwewsewwwwnwwew
wswwwwwwswnewwnwenwwnwww
seseseseneseseswenwsenwseseseseseseswenw
swnwneswwswenwneenwnwnenwenwswwsenwse
senweseswswwesenesenee
wswnesenewwwwwwwwsewwnewwwsew
swnwswwswswseswswnwnwswwsweswweenw
enweseneneswnenwne
esweeeeeeeneweenweeenesee
wneseseseseseseswnwseseswseswswswswsesene
nenwwsenwwswwnwnewnwwnwswnwwenwnww
swswnwswseswswswswswswswswsenwwswnwswe
swnwseswswseseseswwseseswseseneseswswne
neneswwswswseswwwswswnesesenweswsweswse
eneseeneeneweeeneene
seseseseseseseesesesenwwseseseswnesesese
neseenwnenwswswwwwnwewswsenewnwswwne
seswesenwsenesesesewseeseswneenwswne
swnwnwnwnwnwnenwnwnwneswswewwnwenwnwnwnw
eeswswnwnwneswnewneseneewnweneeneene
seneseseeswnewwsenw
neeswseeneenweenweseseeenwewnenee
swswseseswswsesenwswnwseseseswsesw
sesesesenesesesenwswseseeeeenweeesesw
eswwswnwseweewwswwnewnwwwwwnw
enwnwnenwswnwwnesweenwwnwsenwnwnene
esenwnewweswsewneeewseewswese
eswwwenewseene
seswneswwswswswswswswneswswswswswswswsew
eewnwswwwwwnwesewwnesenwwnwese
seneeeswewneenwnenenenenewneenwe
enenwnesenwnwsenwnwnwnwwnwwnwnenwnwnw
seswwseseneswseseseswsesenesesenwse
eneeseneneewenesewwewwsewsenew
swsenwseeswswswswnwswseswswseswswswese
eeeeneeeseneenweneeene
swswneswswwwswenwwsweswswswswswswnw
wwnesewwnwwwwnwwsewneswwnewsww
swneeswwnenwseenenwwnwnwnwsenenwnene
nesewneseneeeneeeneeeeneneenew
wneewwnwwswnwswneew
eeeneneneeeeweeneeenene
eeesweeeenweswwnweee
nenwnewwnewswnwnwnesenwseswnwnwsewww
wseeswnenwnenenenwnwnwnwswnwswwnwnee
enweeeseswweneeseeewnweeesesee
nwnwwnewwwenwwesewwwewnwswnw
swseweeeneneeneswnwnenene
nenwnwnesewnwsewnwnwwwwnwwnwnwnwsww
eeeweeenweeseeenweswnenee
senesesewwnesenesweweneeewsewee
seneswnwnenewseneseseswsenweseswseseswese
swwswwwnenenwwwwwswswsweswwswsese
esweenweneweeeseeeeweeee
eseseeseseseseseseseneswseswsenwse
wenwwnwsewwwwswsewwwwwnwnwnwnw
wnwwwewwenwww
swswseseseswnwswswsenwseswseseswseseswnw
wnwswswswswwswswswwswswwsesw
nesenenewneswewnenwnwnwsenesenenenenwne
sesesenwseswseswseseese
swseseseseeenwnweseesewenesw
ewneswnenenenenenenesenenenenenenewe
swswswswwwsweseswswnwswwnwseswswwsw
esweeeeenweeeeneswe
wwwsewwwnewwwnwnewwwwwsenww
wnwseseneseswsenwswseswneseeswnwswwnee
swswswswnwswneswseswseswswswseswswnwswsw
neeswnewnesenenewenwewneeneenene
swewswnweewswswswnwswweeenwswne
sesenwnwnwnwwnesenwswwnwnwnewnwnwwnwe
wwswwswnesenewwnwwwwwsweswww
nenewnwsweneswswenwweenwsenwnesesww
seneesesesenwswswnwseseswseesenwswwnwsw
nweneeseswweeeneeeseeeweswe
swnenwneenenwwneneenwnesenewnenwnenwsw
wnwnwnesenwnwnwnwwsenwnenwwswnwnwnwnw
nwswnwnwseeeswwsenwwsewe
nwnweenenwneseswnwnwsewwewwsenwnwnw
nwneneneswneneneenenenenwnenwswnwnwswne
nenenwswnenwnenwnwneenenenwnenwnenesese
nwsesesesenwsesesesewseseseseseseseneesw
wnweeeswseneeenenwseewwswsewsee
ewneeeneeeeswnesenweswswnweene
eenweswwnweseeenwseeenenwswswne
swnwwswswseeswsewwwwsewneneewne
neeneswsenwwnwneswwsweewnenweswswnee
nenenenwswswewswnenwenesenwnwneenwnenw
swwsweewwwwswnesw
sweeeeeeenenesweeeeenenwe
seswwseseeeswnwneneswswswewwswswsw
eseesweeesenweeesewsweseewnwe
swswsewewwweenenwnwswneswsw
swswsesenesesenesese
eeeneeeeneswneswenenwenwneenenene
swneneewwsenenenwneeseewsenenenee
wwswwseswwswnwneeenewseswnew
neswwwenwnwswwwwenwneswswweeswnw
wwwwwwewwwwnwswwwwneseww
nwnesenenenwnwnwnwwnwnenwnwnwwenwnwnw
nenenenenwneneewsenwnwneswnwnenenwnene
senwnwnenenenwnwwnwenenenwnenwnwnenww
nenesenenewneneneneneswnenenenenenwnene
eeswnwnwswseswnweswneswnweseswsesewswse
swnesenenwnenewnenenenwnenenwnenwnwnwe
swswnwenwnwnwwswnwnenwnwneswsenwenwww
nwnwnwnesewnenwnenenwnwnwnwenenwnwwnw
swswwwneswswsweswswswswswew
nenwnwseewnwwenwnwsenenwsewnww
nesewnesenenwnewsenewneneeneseneeneesw
nwswnwwswswwnwewswswesweswweww
neneswnenenenenenenenene
neswneneeeneeeeseneeneenew
eneeweneneneeeeeenesewweeeese
sesweseneswnwswnewswnewneswseswne
seneseeswsesewseseseswnwsesesenwsweswsese
swnweneseweeswnwseeewseeeesesesw
swneseseswswswsewswswswswseseswswnesww
nwswseseswseeseswswswswswseswneseseswsw
swswswnewswwnwnwswnwswseweneswswsee
swswseswswswseswswnwswswnesweseswseswsw
nenesenenesenwnenwwnenewsenenesenenenene
nwnwnwswnwnenwnwnwnwnwnwnenwnwnwnwwnwsee
newnewnenewesewswnenesweenwnenenene
seseseseewenwseenwneseesesewnwsesese
seeeeseweeesewseeeeeeenwene
wwswesweswnewsewswnwswweeswswswnw
wswewwwwwwwwwsww
swwewnwnwnwnenewswneseseenenwsenese
swnenwnweewnweesenewneswseeswsene
eseeeseswwnwseneseeseseseseseeseese
swnwnwneeneseneswwneneeneeneeswwse
nwnwnenweswnwswnwnwnwnwnenwnwnwnwenwnw
swsesenwnwswswnwesesesenwnwswsweneswnw
nesenwneenwnenenenwneneswwnwnwnwnwew
nwnwnwsenwnwnwneswnenwnwnwnwnenwsenwnwnwnw
nenwewsewsenwnwnwwnwnwnwnwwswnwnwnw
nenenenesewnenweneeneseneenenenwnesw
neeeseneeeewweeneneeeneweene
sewwswswswswswnwswneeswseeswsweswnw
wswswswneswswswsweseswswswswswswswswnesew
eswwseseneeneeeeneeeeeewwee
swsewswsenwsesenesesesesesesesesesesesw
eeewswseeeseeweeeeeeeneenwe
esesewenweeweeseneeseswseeeee
neseeneeweneeneneneneenenewnenene
wwnwnenwnwnweswnewswnwwnwnw
nwswenwnenwnwnwnwnwnenwnwswwenwenwnw
swnwnwswnwnweswenwewnw
wwneseseneseneswswnesenewwsesweswsese
ewneseneneswenwnwnenwneneenewesesw
nwsewswnwswseswwenwseneswseeneneene
enenenwnenwsenwnwnenwnenewnenwnenwwswnw
eseeesweeswnweneseeeeeeeese
neneswswnwwswneneneneneseenenwswee
wwwwwewwwwswwnewewwwwnw
nenesenwwnenenenenenwnwnenene
eeseeeeswsesewenenwenweewsee
neseneneneneneeneswenenenwneswnw
newswneseewnwwwnenwsenweenweswsw
nenwenwneeeswnweneneneeneswswswnenesw
neseseeneewneswseeswnwswwswenwnwswsw
esenenweneswswnwsewnwneneneeswnwnwswne
wwnewnewsewwwwnwwwswwwwsenew
neswsewsewswwwwenwwswwwswnwnwnese
wnwswwewewnwewwsweswwnwswsesww
nwsweseneseenwswwseseswswwseeneswswsw
nenesweweneeseneneneeneneewenene
nwnwwenwnewwswwwnwnewwwwnwwsww
wenesewsweeeseenenweenwseeee
swswsenenwwswwwswwswswewsww
seseseseswseeneeseneenwseeswweseenw
esewenwwnenwewenenwswseesesewsew
wenenwseneseneswseswsewewseswesenenenw
neswswesenwswswneeswneswswswswewswnww
wswswswswswswwneeswwswswswwswneswsw
swswswneswnwnwseswswswswseswswseseswswsw
nwswseeeeweeweeeeeeeeee
nwswnwnwnwwnwwwnwnwwnwwsenenwnenwe
eneseneeeeesweeeeweenew
wnwwnewnwnwsewnwwwnesenwwnwwswnw
eswewseswsweswnwseseswesewswwwwe
swwswswsesesenwseswesweswseseswnw
"""
