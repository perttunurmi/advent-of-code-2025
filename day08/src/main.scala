import scala.io.Source
import scala.collection.mutable.ListBuffer

class Vector3(val x: Long = 0, val y: Long = 0, val z: Long = 0) {
  override def equals(other: Any): Boolean = other match {
    case vector: Vector3 =>
      this.x == vector.x && this.y == vector.y && this.z == vector.z
    case _ => false
  }

  override def toString(): String = {
    return f"$x $y $z"
  }
}

// Collection of disjoint (non-overlapping) sets
class DisjointSet(var sets: Set[Set[Vector3]]) {
  override def toString(): String = {
    var str = new StringBuilder()
    var count = 1

    sets.foreach(set => {
      str.append(f"\nPrinting the set number ${count}\n")
      set.foreach(c => str.append(f"$c\n"))
      count += 1
    })

    str.toString
  }

  def addSet(set: Set[Vector3]): Unit = {
    val (overlapping, nonOverlapping) = sets.partition(v => {
      v.intersect(set).nonEmpty
    })
    val mergedSet = overlapping.foldLeft(set)(_ ++ _)
    this.sets = nonOverlapping + mergedSet
  }

  def mergeTwoSets(S: Set[Vector3], A: Set[Vector3]): Unit = {
    var combined = S ++ A
    addSet(combined)
  }

  def getDistancesBetweenSets(): List[(Set[Vector3], Set[Vector3], Double)]  = {
    (for {
      S <- this.sets
      A <- this.sets
      if S != A
    } yield (S, A, minDistanceBetweenSets(S, A))).toList
  }

  def connectTwoClosest(): Unit = {
    var sets = getDistancesBetweenSets()
      .sortBy(_._3)
      .head
    addSet(sets._1 ++ sets._2)
  }
}

def d(a: Vector3, b: Vector3): Double = {
  var dv = (b.x - a.x, b.y - a.y, b.z - a.z)
  math.sqrt(
    dv(0) * dv(0) +
    dv(1) * dv(1) +
    dv(2) * dv(2)
  )
}

def minDistanceBetweenSets(A: Set[Vector3], S: Set[Vector3]): Double = {
  minDistanceBetweenSetsVector(A, S)(2)
}

def minDistanceBetweenSetsVector(A: Set[Vector3], S: Set[Vector3]): (Vector3, Vector3, Double)= {
  (for {
    v <- A
    w <- S
  } yield (v, w, d(v, w)))
    .toList
    .sortBy(d => d._3)
    .head
}

def getCoordinates(input: List[String]): List[Vector3] = {
    input.map(pos => {
      var a = pos.split(","); Vector3(a(0).toLong, a(1).toLong, a(2).toLong)
    })
}

def getDistances(positions: List[Vector3], len: Int) = {
  positions.zipWithIndex
    .map((a, i) => {
      var list = ListBuffer[(Vector3, Vector3, Double)]()
      (i + 1 to positions.length - 1).foreach(j => {
        var b = positions(j)
        if (i != j) list.addOne((a, b, d(a, b)))
      })
      list.toList })
      .flatten
      .sortBy(f => f._3)
      .slice(0, len)
}

def getDistances(positions: List[Vector3]) = {
  positions.zipWithIndex
    .map((a, i) => {
      var list = ListBuffer[(Vector3, Vector3, Double)]()
      (i + 1 to positions.length - 1).foreach(j => {
        var b = positions(j)
        if (i != j) list.addOne((a, b, d(a, b)))
      })
      list.toList })
      .flatten
      .sortBy(f => f._3)
}


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
    val selectedLen = if (input.size >= 1000) 1000 else 10

    var positions = getCoordinates(input)
    var distances = getDistances(positions, selectedLen)

    var edges = distances.map(f => (f(0), f(1))).toSet
    var nodes = edges.map(f => Set(f(0), f(1))).flatten.toList

    var dsu = DisjointSet(Set(Set(nodes(0))))

    edges.foreach(v => {
      dsu.addSet(Set(v(0), v(1)))
    })

    solution = dsu
      .sets
      .toList
      .sortBy(S => S.size)
      .reverse
      .zipWithIndex
      .takeWhile((_,i) => i < 3)
      .foldLeft(1)((size, S) => size * S(0).size)

    return solution.toString()
  }

  def solveSecond(input: List[String]): String = {
    var solution: Long = 0
    val selectedLen = if (input.size >= 1000) 1000 else 10

    var positions = getCoordinates(input)
    var distances = getDistances(positions).sortBy(_._3)

    var edges = distances.map(f => (f(0), f(1))).toSet
    var nodes = edges.map(f => Set(f(0), f(1))).flatten.toList

    var dsu = DisjointSet(Set(Set(nodes(0))))

    nodes.foreach(v => {
      dsu.addSet(Set(v))
    })

    var sets = Set[Set[Vector3]]()
    while (dsu.sets.size > 1) {
      println(dsu.sets.size)
      sets = dsu.sets
      dsu.connectTwoClosest()
    }

    var v = minDistanceBetweenSetsVector(sets.head, sets.last)
    solution = v._1.x * v._2.x

    return solution.toString()
  }
}
