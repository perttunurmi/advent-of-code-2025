import scala.io.Source
import scala.collection.mutable.ListBuffer

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
      input = input :+ (line)
    }

    return input
  }
}

object Solution {
  def solveFirst(input: List[String]): String = {
    var solution: Long = 0

    input.foreach(bank => {
      var largest: Long = 0
      (0 to bank.length - 1).foreach(ii => {
        (ii to bank.length - 1).foreach(jj => {
          if (ii != jj) {
            if (
              (bank(ii).toString.concat(bank(jj).toString)).toLong > largest
            ) {
              largest = (bank(ii).toString.concat(bank(jj).toString)).toLong
            }
          }
        })
      })
      solution += largest
    })

    return solution.toString()
  }

  def solveSecond(input: List[String]): String = {
    var solution: Long = 0

    input.foreach(bank => {
      var largest: String = ""
      var largestIndex = -1

      (1 to 12).foreach(n => {

        var found = false
        var maxIndex = bank.length - (12 - largest.length)

        bank.zipWithIndex
          .sortBy((c,i) => -c)
          .foreach((value, index) => {
            if (!found && index > largestIndex && index <= maxIndex) {
              largest = largest.concat(value.toString)
              largestIndex = index
              found = true
            }
          })

      })
      assert(largest.length == 12)
      solution += largest.toLong
    })

    return solution.toString()
  }
}
