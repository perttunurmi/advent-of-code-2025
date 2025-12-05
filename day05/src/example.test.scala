//> using test.dep org.scalameta::munit::1.2.1

import Solution.solveFirst
import Solution.solveSecond
import InputHandler.readInputFromFile

class SolutionTests extends munit.FunSuite {
  val input = InputHandler.readInputFromFile("./example.txt")

  test("Example solution 1") {
    val solution = "3"
    assertEquals(solveFirst(input), solution)
  }

  test("Example solution 2") {
    val solution = "14"
    assertEquals(solveSecond(input), solution)
  }
}
