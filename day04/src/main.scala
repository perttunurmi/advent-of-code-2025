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
    var solution = 0

    var listb = ListBuffer[List[Char]]()
    input.foreach(line => listb.addOne(line.toList))
    val col = listb.toList

    col.zipWithIndex.foreach((row, y) => {
      row.zipWithIndex.foreach((c, x) => {
        var rolls = 0
        if (c == '@') {

          if (x + 1 < row.length) if (col(y)(x + 1) == '@') rolls += 1
          if (x + 1 < row.length && y + 1 < col.length)
            if (col(y + 1)(x + 1) == '@') rolls += 1
          if (y + 1 < col.length) if (col(y + 1)(x) == '@') rolls += 1
          if (0 < x && y + 1 < col.length)
            if (col(y + 1)(x - 1) == '@') rolls += 1
          if (0 < x) if (col(y)(x - 1) == '@') rolls += 1
          if (0 < x && 0 < y) if (col(y - 1)(x - 1) == '@') rolls += 1
          if (0 < y) if (col(y - 1)(x) == '@') rolls += 1
          if (0 < y && x + 1 < row.length)
            if (col(y - 1)(x + 1) == '@') rolls += 1

          if (rolls < 4) {
            solution += 1
            // print("x")
          } else {
            // print("@")
          }
        } else {
          // print(".")
        }

      })
      // println()
    })

    return solution.toString()
  }

  def solveSecond(input: List[String]): String = {
    var solution = 0

    var col = ListBuffer[List[Char]]()
    input.foreach(line => col.addOne(line.toList))
    var found = ListBuffer[Tuple2[Int, Int]]()
    var newfound = true

    while (newfound) {
      newfound = false
      col.zipWithIndex.foreach((row, y) => {
        row.zipWithIndex.foreach((c, x) => {
          var rolls = 0
          if (c == '@') {

            if (x + 1 < row.length)
              if (col(y)(x + 1) == '@' && !found.contains((x + 1, y))) {
                rolls += 1
              }

            if (x + 1 < row.length && y + 1 < col.length)
              if (col(y + 1)(x + 1) == '@' && !found.contains((x + 1, y + 1))) {
                rolls += 1
              }

            if (y + 1 < col.length)
              if (col(y + 1)(x) == '@' && !found.contains((x, y + 1))) {
                rolls += 1
              }

            if (0 < x && y + 1 < col.length)
              if (col(y + 1)(x - 1) == '@' && !found.contains((x - 1, y + 1))) {
                rolls += 1
              }

            if (0 < x)
              if (col(y)(x - 1) == '@' && !found.contains((x - 1, y))) {
                rolls += 1
              }

            if (0 < x && 0 < y)
              if (col(y - 1)(x - 1) == '@' && !found.contains((x - 1, y - 1))) {
                rolls += 1
              }

            if (0 < y)
              if (col(y - 1)(x) == '@' && !found.contains((x, y - 1))) {
                rolls += 1
              }

            if (0 < y && x + 1 < row.length)
              if (col(y - 1)(x + 1) == '@' && !found.contains((x + 1, y - 1))) {
                rolls += 1
              }

            if (rolls < 4 && !found.contains((x, y))) {
              found.addOne((x, y))
              solution += 1
              newfound = true
            } else {
              // print("@")
            }
          } else {
            // print(".")
          }

        })
        // println()
      })
    }

    return solution.toString()
  }
}
