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
    var rotation = 50
    
    input.foreach(line => {
      line(0) match
        case 'R' => { rotation += line.drop(1).toInt }
        case 'L' => { rotation -= line.drop(1).toInt }
        case _ => println(_)

       if (rotation % 100 == 0) { solution = solution + 1 }
    })
    
    return solution.toString()
  }

  def solveSecond(input: List[String]): String = {
    var solution = 0
    var rotation = 50
    
    input.foreach(line => {
      line(0) match
        case 'R' => {
            var rotationAmount = line.drop(1).toInt
            
            (1 to rotationAmount).inclusive.foreach(_ => {
              rotation += 1
              if (rotation % 100 == 0) { solution += 1 }
            })
           }

        case 'L' => {
            var rotationAmount = line.drop(1).toInt

            (1 to rotationAmount).inclusive.foreach(_ => {
              rotation -= 1
              if (rotation % 100 == 0) { solution += 1 }
            })
           }

        case _ => println(_)
    })

    return solution.toString()
  }
}
