import scala.io.Source
import scala.language.postfixOps
import scala.reflect.ClassTag

class OhlapnaOblika(Ni: Array[Int], Bi: Array[Int], Ai: Array[Array[Double]], bi: Array[Double], ci: Array[Double], vi: Double) {
  var N = Ni
  var B = Bi
  var A = Ai
  var b = bi
  var c = ci
  var v = vi

  def copy(): OhlapnaOblika = {
    new OhlapnaOblika(Array.ofDim[Int](N.length), Array.ofDim[Int](B.length), Array.ofDim[Double](A.length, A(0).length), Array.ofDim[Double](b.length), Array.ofDim[Double](c.length), 0)
  }

  def deepcopy(): OhlapnaOblika = {
    new OhlapnaOblika(N, B, A, b, c, v)
  }
  
  def mcopy(): OhlapnaOblika = {
    new OhlapnaOblika(Array.ofDim[Int](0), Array.ofDim[Int](0), Array.ofDim[Double](A.length, A(0).length), Array.ofDim[Double](b.length), Array.ofDim[Double](c.length), 0)
  }
}

object OhlapnaOblika {
  def apply(N: Array[Int], B: Array[Int], A: Array[Array[Double]], b: Array[Double], c: Array[Double], v: Double) = new OhlapnaOblika(N, B, A, b, c, v)

  def apply(self: OhlapnaOblika) = self.copy
}

def removeAtIdx[T: ClassTag](A: Array[T], index: Int): Array[T] = {
  var ret = Array.ofDim[T](A.length - 1)
  for (i <- A.indices) {
    if (i > index) {
      ret(i - 1) = A(i)
    } else if (i < index) {
      ret(i) = A(i)
    }
  }
  ret
}

def addToArray[T: ClassTag](a: Array[T], nele: T): Array[T] = {
  var ret = Array.ofDim[T](a.length + 1)
  for (i <- a.indices) {
    ret(i) = a(i)
  }
  ret(a.length) = nele
  ret
}

/**
 * O(getNonZeroIndex) = 2n + 1
 */
def getNonZeroIndex(N: Array[Int], c: Array[Double]): Option[Int] = {
  for (j <- N.indices) {
    val i = N(j)
    if (c(i) > 0) return Some(j)
  }
  None
}

/**
 * O(getMinIndex) = 3 + 3m
 */
def getMinIndex(A: Array[Double]): Int = {
  var min = A(0)
  var index = 0
  for (i <- 1 until A.length) {
    if (min > A(i)) {
      min = A(i)
      index = i
    }
  }
  index
}

/**
 * O(initSimplex) = O(getMinIndex) + O(initArray(n)) + O(initArray(m)) + n + m
 * O(initSimplex) = O(m + n) + 1 + 1 + n + m
 * O(initSimplex) = 2n + 2m
 */
def initSimplex(A: Array[Array[Double]], b: Array[Double], c: Array[Double], n: Int, m:Int): Option[OhlapnaOblika] = {
  val l = getMinIndex(b)
  if (b(l) >= 0) {
    var N = Array.ofDim[Int](n)
    for (i <- 0 until n) N(i) = i
    var B = Array.ofDim[Int](m)
    for (i <- 0 until m) B(i) = m + i
    Some(OhlapnaOblika(N, B, A, b, c, 0))
  } else {
    None
  }
}

/**
 * O(pivot) = O(mcopy) + O(removeAtIdx(n)) + O(removeAtIdx(m)) + 3 + n - 1 + 1 + (m - 1) * (2 + n - 1) + 1 + n - 1 + 4
 * O(pivot) = O(mcopy) + O(removeAtIdx(n)) + O(removeAtIdx(m)) + 7 + n + 2m + mn - m - 2 - n + 1
 * O(pivot) = O(mcopy) + O(removeAtIdx(n)) + O(removeAtIdx(m)) + 6 + m + mn
 * O(pivot) = 1 + n + 1 + m + 1 + 6 + m + mn
 * O(pivot) = n + m + 9 + m + mn
 * O(pivot) = n + 2m + 9 + mn
 * O(pivot) = mn
 */
def pivot(oi: OhlapnaOblika, ei: Int, li: Int): OhlapnaOblika = {
  var o = oi.mcopy
  val N = removeAtIdx(oi.N, ei)
  val B = removeAtIdx(oi.B, li)
  val e = oi.N(ei)
  val l = oi.B(li)
  o.b(e) = oi.b(l) / oi.A(l)(e)
  for (i <- N) o.A(e)(i) = oi.A(l)(i) / oi.A(l)(e)
  o.A(e)(l) = 1 / oi.A(l)(e)
  for (i <- B) {
    o.b(i) = oi.b(i) - oi.A(i)(e) * o.b(e)
    for (j <- N) {
      o.A(i)(j) = oi.A(i)(j) - oi.A(i)(e) * o.A(e)(j)
    }
    o.A(i)(l) = - oi.A(i)(e) * o.A(e)(l)
  }
  o.v = oi.v + oi.c(e) * o.b(e)
  for (i <- N) {
    o.c(i) = oi.c(i) - oi.c(e) * o.A(e)(i)
  }
  o.c(l) = - oi.c(e) * o.A(e)(l)
  o.N = addToArray(N, l)
  o.B = addToArray(B, e)
  o
}

/**
 * O(simplex) = O(initSimplex) + 1 + n * (n + 1 + 3m + O(getMinIndex(m)) + 1 + O(pivot)) + O(initArray(m + n)) + (m + n) * 2 + 2
 * O(simplex) = O(initSimplex) + 3 + nn + n + 3mn + n * O(getMinIndex(m)) + n + n * O(pivot) + O(initArray(m + n)) + 2m + 2n 
 * O(simplex) = 2n + 2m + 3 + n^2 + n + 3mn + nm + n + n * nm + 2m + 2n 
 * O(simplex) = 6n + 4m + 3 + n^2 + 4mn + mn^2
 * O(simplex) = mn^2 + n^2 + 4nm + 6n + 4m + 3
 */
def simplex(Ai: Array[Array[Double]], bi: Array[Double], ci: Array[Double], n: Int, m: Int): Option[Array[Double]] = {
  val oi = initSimplex(Ai, bi, ci, n, m)
  if (!oi.isDefined) return None
  var o = oi.get
  while (getNonZeroIndex(o.N, o.c).isDefined) {
    val ei = getNonZeroIndex(o.N, o.c).get
    val e = o.N(ei)
    var delta = Array.ofDim[Double](o.B.length)
    for (i <- delta.indices) {
      val j = o.B(i)
      if (o.A(j)(e) > 0) delta(i) = o.b(j) / o.A(j)(e)
      else delta(i) = Double.PositiveInfinity
    }
    val li = getMinIndex(delta)
    if (delta(li) == Double.PositiveInfinity) return None
    else o = pivot(o, ei, li)
  }
  var ret = Array.ofDim[Double](o.b.length + 1)
  for (i <- 0 until o.b.length) {
    if (o.B contains i) ret(i) = o.b(i)
    else ret(i) = 0
  }
  ret(o.b.length) = o.v
  Some(ret)
}

def solveSimplexFile(fileName: String): Unit = {
  val itL = Source.fromFile(fileName).getLines.toList.iterator
  if (itL.hasNext) {
    val tab = itL.next.split("\\s+")
    val n = tab(0).toInt
    val m = tab(1).toInt
    val size = n + m
    itL.next
    var A = Array.ofDim[Double](size, size)
    for (i <- 0 until size) {
      var j = 0
      for (num <- itL.next.split("\\s+")) {
        A(i)(j) = num.toDouble
        j += 1
      }
    }
    itL.next
    var i = 0
    var b = Array.ofDim[Double](size)
    for (num <- itL.next.split("\\s+")) {
      b(i) = num.toDouble
      i += 1
    }
    itL.next
    i = 0
    var c = Array.ofDim[Double](size)
    for (num <- itL.next.split("\\s+")) {
      c(i) = num.toDouble
      i += 1
    }
    val ans = simplex(A, b, c, n, m)
    if (ans != None) println(ans.get.mkString(" ")) else println("No solution")
  } else {
    println("Datoteka " + fileName + " format error!!!")
  }
}

println; solveSimplexFile("test1.txt")
println; solveSimplexFile("test2.txt")
println; solveSimplexFile("test3.txt") 

val A = Array(
  Array(0.0, 0, 0, 0, 0, 0),
  Array(0.0, 0, 0, 0, 0, 0),
  Array(0.0, 0, 0, 0, 0, 0),
  Array(1.0, 1, 1, 0, 0, 0),
  Array(60.0, 80, 120, 0, 0, 0),
  Array(6.0, 4, 5, 0, 0, 0)
  )
val b = Array(0.0, 0, 0, 70, 6000, 330)
val c = Array(80.0, 95, 110, 0, 0, 0)
val ans = simplex(A, b, c, 3, 3)
println; if (ans != None) println(ans.get.mkString(" ")) else println("No solution")

