import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    (args) foreach (println)

    val input = InputHandler.readInputFromFile("./data.txt")

    println("Solution 1:")
    println(Solution.solveFirst(input))

    println("\nSolution 2:")
    println(Solution.solveSecond(input))
  }
}

object InputHandler {
  def readInputFromFile(filename: String): List[String] = {
    var input = List[String]()
    for (line <- Source.fromFile(filename).getLines()) {
      input = input.appended(line)
    }

    return input
  }
}

object Solution {
  def solveFirst(input: List[String]): String = {
    var solution = 0

    return solution.toString()
  }

  def solveSecond(input: List[String]): String = {
    var solution = 0

    return solution.toString()
  }

}
