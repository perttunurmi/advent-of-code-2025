import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Range.Inclusive

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

    var ranges = ListBuffer[String]()
    var ids = ListBuffer[String]()

    var atRanges = true
    input.foreach(line => {
      if (atRanges) {
        if (line.isEmpty()) { atRanges = false }
        else
          ranges.addOne(line)
      } else {
        ids.addOne(line)
      }
    })

    var freshIds = ListBuffer[Long]()
    ranges.foreach(f => {
      var rstart = f.split("-")(0).toLong
      var rend = f.split("-")(1).toLong
      ids.foreach(id => {
        if (
          rstart <= id.toLong &&
          id.toLong <= rend &&
          !freshIds.contains(id.toLong)
        ) {
          solution += 1
          freshIds.addOne(id.toLong)
        }
      })
    })

    return solution.toString()
  }

  def solveSecond(input: List[String]): String = {
    var solution: BigInt = 0

    var ranges = ListBuffer().addAll(
      input
        .takeWhile(!_.isEmpty())
        .map(line => {
          var start = line.split("-")(0)
          var end = line.split("-")(1)
          (BigInt(start) to BigInt(end))
        })
    )

    var new_overlap_found = true
    while (new_overlap_found) {
      new_overlap_found = false

      println(ranges)

      (0 to ranges.length - 1).foreach(i => {
        (i to ranges.length - 1).foreach(j => {
          if (i != j && !new_overlap_found) {

            var first = ranges(i).start
            var last = ranges(i).end
            var a = ranges(j).start
            var b = ranges(j).end

            if (a <= first && last <= b) {
              ranges.remove(i)
              new_overlap_found = true

            } else if (first <= a && b <= last) {
              ranges.remove(j)
              new_overlap_found = true

            } else if (a <= first && b <= last && first <= b) {
              ranges.update(i, (a to last))
              ranges.remove(j)
              new_overlap_found = true

            } else if (first <= a && last <= b && a <= last) {
              ranges.update(i, (first to b))
              ranges.remove(j)
              new_overlap_found = true
            }
          }
        })
      })
    }

    ranges.map(f => f.end - f.start + 1).foreach(n => solution += n)

    return solution.toString()
  }
}
