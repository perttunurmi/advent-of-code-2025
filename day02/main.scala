import scala.io.Source
import scala.compiletime.ops.int
import scala.collection.mutable.ListBuffer
import scala.compiletime.ops.string

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
  var invalidIds = ListBuffer[Long]()

  def parseInput(input: List[String]): List[Long] = {
    var listOfIds = ListBuffer[Long]()

    input.foreach(line => {
      line.split(",").foreach(range => {
        var start = range.split("-")(0).toLong
        var end = range.split("-")(1).toLong
        (start to end).foreach(number => {
          listOfIds += number
        })
      })
    })

    return listOfIds.toList
  }


  def solveFirst(input: List[String]): String = {
    var invalidIds = ListBuffer[Long]()
    var solution: Long = 0
    var listOfIds = parseInput(input)
    
    listOfIds.foreach(id => {
      var num: String = id.toString()

      if (num.length % 2 == 0) { 
        val splits = num.splitAt(num.length/2)
        if (splits(0) == splits(1)) { invalidIds += id }
      }
    })

    invalidIds.foreach(n => {
      solution += n
    })

    return solution.toString()
  }


  /**
   * Split a @string into substring of size @n and return list of substrings
   */
  def splitString(string: String, n: Int): List[String] = {
    if (string.isEmpty) Nil
    else {
      val (s1, s2) = string.splitAt(n)
      s1 :: splitString(s2, n)
    }
  }

  def findRepeatingPattern(number: Long): Boolean = {
    var num: String = number.toString()
    var isInvalid = false

    (1 to num.length).foreach(n => {
        var list = splitString(num, n)
        if (!isInvalid) {
          isInvalid = list.forall{c => {
            c == list(0) && list.length > 1
          }}
        }
    })
    
    return isInvalid
  }

  def solveSecond(input: List[String]): String = {
    var invalidIds = ListBuffer[Long]()
    var solution: Long = 0
    var listOfIds = parseInput(input)
    
    listOfIds.foreach(id => {
      if (findRepeatingPattern(id)) { invalidIds += id}
    })

    invalidIds.foreach(number => {
      solution += number
    })

    return solution.toString()
  }
}
