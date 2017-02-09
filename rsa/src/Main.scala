import ia.vaja1._
import ia.vaja2.KriptiranjeRSA

object Main {

	def main(args: Array[String]): Unit = {
//		main_vaja1()
		main_vaja2()
	}

	def main_vaja2(): Unit = {
		val rsa1 = new KriptiranjeRSA
		rsa1.zapisiKljuce
		val list = List(BigInt(100), BigInt(200), BigInt(300), BigInt(400), BigInt(500))
		val listCript = rsa1.kriptirajPodatke(list)
		println("Kriptirani podatki: " + listCript + "\n" + rsa1.dekriptirajPodatke(listCript) + " == " + list)
		rsa1.zapisiPodatke(list)
		val rsa2 = new KriptiranjeRSA
		val listOut = rsa2.beriPodatke
		println("Dekriptiranje podatkov: " + listOut + " == " + list + "\n")
	}

   def main_vaja1(): Unit = {
      val rand = new PrimeRandomGenerator()
      println(rand.testNaive(5) + "\n" + rand.testNaive(1847) + "\n" + rand.testNaive(1860) + "\n" + rand.testNaive(1000003))
      println(rand.testMilerRabin(5, 4) + "\n" + rand.testMilerRabin(1847, 3) + "\n" + rand.testMilerRabin(1860, 3) + "\n" + rand.testMilerRabin(1000003, 3))
      println(rand.naive + "\n" + rand.naive)
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



