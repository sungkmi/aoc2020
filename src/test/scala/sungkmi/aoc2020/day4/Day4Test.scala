package sungkmi.aoc2020.day4

class Day4Test extends munit.FunSuite {

  test("parse passport") {
    val s = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm"""
    val expected = Map(
      "ecl" -> "gry",
      "pid" -> "860033327",
      "eyr" -> "2020",
      "hcl" -> "#fffffd",
      "byr" -> "1937",
      "iyr" -> "2017",
      "cid" -> "147",
      "hgt" -> "183cm",
    )
    assertEquals(parsePassport(s), expected)

  }
}
