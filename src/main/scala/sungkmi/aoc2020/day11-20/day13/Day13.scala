package sungkmi.aoc2020.day13

import scala.util.Try

type Timestamp = Long
type Id = Int
type Minute = Long

def parse(s: String): (Timestamp, Seq[Option[Id]]) =
  val Array(line1, line2) = s.split("\n")
  (line1.toLong, line2.split(",").toSeq.map(s => Try(s.toInt).toOption))

def waitingTime(m: Timestamp, id: Id): Timestamp =
  val r = m % id
  if r == 0L then 0 else m + id.toLong - r

def earliestBus(m: Timestamp, ids: Seq[Id]): (Minute, Id) =
  ids.map(id => (waitingTime(m, id) - m, id)).min

def earliestTimestamp(buses: Map[Id, Int]): BigInt =
  val m = buses.keys.map(BigInt(_)).product
  val x = buses.map:
    (m, a) =>
      val n = buses.keys.map(_ % m).map(BigInt(_)).product % m
      val s = (0 until n.toInt).map{ i => (n * i % m, i) }.find(_._1 == 1).get._2
      a * n * s
  .sum
  x % m


@main def part1: Unit =
  val (minTime, ids) = parse(input)
  val (minutes, id) = earliestBus(minTime, ids.flatten)
  val ans = minutes * id
  println(ans)

@main def part2: Unit = 
  val ids = parse(input)._2
  val buses = ids.zipWithIndex.flatMap:
    case (Some(id), index) => Seq(id -> index)
    case (None, _) => Seq.empty
  .toMap
  val ans = earliestTimestamp
  println(ans)

lazy val input: String = """1004345
41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,379,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,557,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19"""
