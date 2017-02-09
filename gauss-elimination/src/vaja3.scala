import scala.io.Source

// O(3)
def swapRow[T](arr: Array[T], r1: Int, r2: Int) {
  val tmp = arr(r1)
  arr(r1) = arr(r2)
  arr(r2) = tmp
}

// O(n^2 + n^3 + n)
def gaussElim(a: Array[Array[Double]], y: Array[Double]): Option[Array[Double]] = {
  for (i <- 0 to a.length - 2) { 
    val maxRow = (i + 1 until a.length).foldLeft(i)((max, r) => if (a(r)(i).abs > a(max)(i).abs) r else max)
    if (a(maxRow)(i) == 0.0) return None 
    swapRow(a, maxRow, i)
    swapRow(y, maxRow, i)
    for (j <- i + 1 to a.length - 1) {
      val factor = a(j)(i) / a(i)(i)
      a(j)(i) = 0.0
      for (k <- i + 1 to a.length - 1) {
        a(j)(k) -= a(i)(k) * factor
      }
      y(j) -= y(i) * factor
    }
  }
  if (a(a.length - 1)(a.length - 1) == 0) return None
  val x = new Array[Double](a.length)
  for (i <- a.length -1 to 0 by -1) {
    x(i) = y(i)
    for (j <- i + 1 to a.length - 1) x(i) -= a(i)(j) * x(j)
    x(i) /= a(i)(i)
  }
  return Some(x)
}

def solveMatrixFromFile(fileName: String): Unit = {
  val itL = Source.fromFile(fileName).getLines.toList.iterator
  if (itL.hasNext) {
    val n = itL.next.toInt
    var A = Array.ofDim[Double](n, n)
    var y = Array.ofDim[Double](n)
    for (i <- 0 to n - 1) {
      var j = 0
      for (num <- itL.next.split("\\s+")) {
        if (j < n) A(i)(j) = num.toDouble else y(i) = num.toDouble
        j += 1
      }
    }
    val ans = gaussElim(A, y)
    if (ans != None) println(ans.get.mkString(" ")) else println("No solution")
  } else {
    println("Datoteka " + fileName + " format error!!!")
  }
}

/*
val A1 = Array(Array(4.0, -2.0, -2.0), Array(8.0, -5.0, 1.0), Array(-2.0, 7.0, -2.0))
val y1 = Array(10.0, 6.0, 3.0)
val ans1 = gaussElim(A1, y1)
if (ans1 != None) println(ans1.get.mkString(" ")) else println("No solution")

val A2 = Array(Array(9.0, 3.0, 4.0), Array(4.0, 3.0, 4.0), Array(1.0, 1.0, 1.0))
val y2 = Array(7.0, 8.0, 3.0)
val ans2 = gaussElim(A2, y2)
if (ans2 != None) println(ans2.get.mkString(" ")) else println("No solution")

val A3 = Array(Array(1.0, 1.0), Array(-1.0, -1.0))
val y3 = Array(3.0, 3.0)
val ans3 = gaussElim(A3, y3)
if (ans3 != None) println(ans3.get.mkString(" ")) else println("No solution")
*/
