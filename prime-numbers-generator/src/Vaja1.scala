object Main {
   def main(args: Array[String]): Unit = {
      val rand = new PrimeRandomGenerator()
      println(rand.testNaive(5))
      println(rand.testNaive(1847))
      println(rand.testNaive(1860))
      println(rand.testNaive(1000003))
      println()
      println(rand.testMilerRabin(5, 4))
      println(rand.testMilerRabin(1847, 3))
      println(rand.testMilerRabin(1860, 3))
      println(rand.testMilerRabin(1000003, 3))
      println()
      println(rand.naive)
      println(rand.naive)
      println()
      var num = rand.milerRabin()
      println(rand.testNaive(num) + " " + num)
      num = rand.milerRabin()
      println(rand.testNaive(num) + " " + num)
      num = rand.milerRabin()
      println(rand.testNaive(num) + " " + num)
      num = rand.milerRabin()
      println(rand.testNaive(num) + " " + num)
      num = rand.milerRabin()
      println(rand.testNaive(num) + " " + num)
      num = rand.milerRabin()
      println(rand.testNaive(num) + " " + num)
      num = rand.milerRabin()
      println(rand.testNaive(num) + " " + num)
   }
}



