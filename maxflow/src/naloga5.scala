import scala.collection.mutable
import scala.io.Source

case class TwoO(var set: Boolean = false, var value: Double = 0.0)

/**
 * Vrne vrednost, ki pove ali obstaja pot od vozlisca d to vozlisca t
 *
 * Casovna zahtevnost:
 *    O(m + n) => m stevilo vozlisc; n stevilo povezav
 *
 * @param A Graf v katermem iscemo obhod
 * @param s Zacetno vozlisce grfa A
 * @param t Koncno vozlisce grafa A
 * @param pot Pot od vozlisca s do vozlisca t, ki jo napolni metoda
 * @return True: ce obstaja obhod iz vozlisca s to vozlisca t
 *         False: v nasprotne primeru
 */
def bfs(A: Array[Array[Double]], s: Int, t: Int, pot: Array[Int]): Boolean = {
  var obiskani = new Array[Boolean](A.length)
  for (i <- obiskani.indices) {
    obiskani(i) = false
  }
  var vrsta = new mutable.Queue[Int]()
  vrsta += s
  obiskani(s) = true
  pot(s) = -1
  while (vrsta.nonEmpty) {
    val u = vrsta.dequeue
    for (v <- obiskani.indices) {
      if (!obiskani(v) && A(u)(v) > 0) {
        vrsta += v
        pot(v) = u
        obiskani(v) = true
      }
    }
  }
  obiskani(t)
}

/**
 * Casovna zahtevnost:
 *    O(m * n) => m stevilo stolpcev; n stevilo vrednosti v stolpcu
 *    O(n * n) v nasem primeru
 *
 * @param A
 * @return
 */
def arrayCopy(A: Array[Array[Double]]): Array[Array[Double]] = {
  var nArray = Array.ofDim[Double](A.length, A(0).length)
  for (i <- A.indices) {
    for (j <- A(i).indices) {
      nArray(i)(j) = A(i)(j)
    }
  }
  nArray
}

/**
 * Casovna zahtevnost:
 *    O(m * n * n) => m stevilo vozlisc; n stevilo povezav
 *
 * @param A graf predstavljen s tabelo
 * @param s zacetno vozlisce
 * @param t koncno vozlisce
 * @return
 */
def fordFulkerson(A: Array[Array[Double]], s: Int, t: Int): (Double, Array[Array[Double]]) = {
  // Rezidencni graf
  var rA = arrayCopy(A)
  var maxPretok = 0.0
  var pot = new Array[Int](A.length)
  while (bfs(rA, s, t, pot)) {
    var rPretok = TwoO()
    var u = 0
    var v = t
    while (v != s) {
      u = pot(v)
      if (!rPretok.set) {
        rPretok.value = rA(u)(v)
        rPretok.set = true
      } else if (rPretok.value > rA(u)(v)) {
        rPretok.value = rA(u)(v)
      }
      v = pot(v)
    }
    v = t
    while (v != s) {
      u = pot(v)
      rA(u)(v) -= rPretok.value
      rA(v)(u) += rPretok.value
      v = pot(v)
    }
    maxPretok += rPretok.value
  }
  (maxPretok, rA)
}

/**
 *
 * @param inFilePath
 * @return
 */
def function(inFilePath: String): String = {
  val itL = Source.fromFile(inFilePath).getLines.toList.iterator
  val stNodes = itL.next.split("\\s+")(0).toInt
  val graph = Array.ofDim[Double](stNodes, stNodes)
  while (itL.hasNext) {
    val rvals = itL.next.split("\\s+")
    graph(rvals(0).toInt)(rvals(1).toInt) = rvals(2).toDouble
  }
  val (maxPretok, rGraph) = fordFulkerson(graph, 0, stNodes - 1)
  println("Maksimalni pretko: " + maxPretok)
  var buffer = new StringBuilder
  for (i <- graph.indices) {
    for (j <- graph(i).indices) {
      if (graph(i)(j) > 0) {
        buffer ++= "(" + i + " , " + j + ") [" + (graph(i)(j) - rGraph(i)(j)) + " / " + graph(i)(j) + "]\n"
      }
    }
  }
  buffer.toString
}

println(function("vhod.txt"))
println(function("test.txt"))
