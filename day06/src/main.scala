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

    var data = input.map(s => s.trim.split("\\s+").mkString(" ").split(" "))
    // println(data)

    data.last.zipWithIndex.foreach((symbol, i) => {
      assume(symbol == "*" || symbol == "+")

      // println(symbol)
      symbol match
        case "*" => {
          var sum: Long = 1
          (0 to data.length-2).foreach(j => {
            // println(data(j)(i).toInt)
            sum = sum * data(j)(i).toLong
          })
          solution += sum
        }

        case "+" => {
          var sum: Long = 0
          (0 to data.length-2).foreach(j => {
            // println(data(j)(i).toInt)
            sum += data(j)(i).toLong
          })
          solution += sum
        }
    })

    return solution.toString()
  }

  def solveSecond(input: List[String]): String = {
    var solution: BigInt = 0

    var cols = input(0).zipWithIndex.map((c, i) => {
      val number = StringBuilder()
      (0 to input.length-2).foreach(j => {
        number.addOne(input(j)(i))
      })

      number.toString
    }).toList

    val operators = input.last.trim.split("\\s+").toList
    val operands = cols.map(s => if (!s.strip.isEmpty()) s.strip else "0")

    var i = 0
    operators.foreach(operator => {
      var combi: BigInt = 0

      operator match
      case "*" => combi = 1
      case "+" => combi = 0
      
      while(i < operands.length && operands(i) != "0" ) {
        operator match
        case "*" => combi = combi * BigInt(operands(i))
        case "+" => combi = combi + BigInt(operands(i))

        i += 1
      }

      i += 1
      solution += combi
    })

    return solution.toString()
  }
}
