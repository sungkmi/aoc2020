package sungkmi.aoc2020.day21

class Day21Test extends munit.FunSuite {

  val testInput = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"""

  val foods = Seq(
    Set("mxmxvkd", "kfcds", "sqjhc", "nhms") -> Set("dairy", "fish"),
    Set("trh", "fvjkl", "sbzzf", "mxmxvkd") -> Set("dairy"),
    Set("sqjhc", "fvjkl") -> Set("soy"),
    Set("sqjhc", "mxmxvkd", "sbzzf") -> Set("fish"),
  ).map:
    case (ingredients, allergens) =>
      ingredients.map(Ingredient(_)) -> allergens.map(Allergen(_))

  test("day21 parse") {
    assertEquals(parse(testInput), foods)
  }

  test("day21 allergenIngrediantCandidates") {
    assertEquals(
      allergenIngrediantCandidates(foods)(Allergen("dairy")),
      Set(Ingredient("mxmxvkd"))
    )
  }

  test("day21 solve1") {
    assertEquals(solve1(testInput), 5)
  }

  test("day21 solve2") {
    assertEquals(solve2(testInput), "mxmxvkd,sqjhc,fvjkl")
  }
}
