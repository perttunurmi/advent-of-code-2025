import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.util.LinkedList

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

    var splits = 0
    var beam = scala.collection.mutable.Set(input(0).indexOf("S"))

    input.foreach(line => {
      var next = scala.collection.mutable.Set[Int]()

      beam.foreach(index => {
        if (line(index) == '^')
          splits += 1
          next.addOne(index - 1)
          next.addOne(index + 1)
        else next.addOne(index)
      })

      beam = next
    })

    solution = splits
    return solution.toString()
  }

  def solveSecond(input: List[String]): String = {
    var solution: BigInt = 0

    var splits: BigInt = 0
    var beam = Set((input(0).indexOf("S"), BigInt(1)))

    var ln =0
    input.foreach(line => {
      var next = scala.collection.mutable.ListBuffer[(Int, BigInt)]()

      beam.foreach((index, count) => {
        if (line(index) == '^') {
          splits += BigInt(1) * count
          next.addOne((index - 1, count))
          next.addOne((index + 1, count))
        } else { next.addOne(index, count) }
      })

      beam = (next.map((index, count) => {
        var realCount: BigInt = 0
        next.foreach((i, c) => {
          if (index == i) {
            realCount = realCount + c
          }
        })

        (index, realCount)
      })).toSet

    })

    beam.foreach((i, c) => solution += c)
    return solution.toString()
  }
}
