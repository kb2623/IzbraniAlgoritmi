package ia.vaja2

import java.io.FileWriter

import scala.io.Source
import scala.collection.mutable.MutableList

import ia.MathBI
import ia.vaja1.PrimeRandomGenerator

class KriptiranjeRSA (
		var keysPath: String = "kljuci.txt",
		var messPath: String = "sporocilo.txt",
		var keysLen: BigInt = 64
) {
	var (e, s, n) = init()

	def this() = this("kljuci.txt", "sporocilo.txt", 64)

	def init(): (BigInt, BigInt, BigInt) = {
		val randGen = new PrimeRandomGenerator()
		randGen.m = MathBI.exp(2, keysLen)
		var redo = false
		var (e, s, n) = naloziKljuce(keysPath)
		if (e > 0 && s > 0 && n > 0) {
			return (e, s, n)
		}
		do {
			var (p, q) = (BigInt(0), BigInt(0))
			do {
				p = randGen.milerRabin
				q = randGen.milerRabin
			} while (p <= 1 || q <= 1)
			val eulerN = (p - 1) * (q - 1)
			n = p * q
			e = randGen.milerRabin
			while (e <= 1 && e > eulerN && MathBI.gcd(e, eulerN) != 1) {
				e = randGen.milerRabin
			}
			val optd = MathBI.modLinearEqu(e, 1, eulerN)
			if (optd == null || e <= 1 || n < 1) {
				redo = true
			} else {
				s = optd.asInstanceOf[BigInt]
				if (s <= 1) {
					redo = true
				} else {
					redo = false
				}
			}
		} while (redo)
		return (e, s, n)
	}

	def naloziKljuce(kljuciPath: String): (BigInt, BigInt, BigInt) = {
		var (e, s, n) = (BigInt(0), BigInt(0), BigInt(0))
		try {
			for (line <- Source.fromFile(keysPath).getLines) {
				val words = line.split(" ")
				e = BigInt.apply(words(0))
				s = BigInt.apply(words(1))
				n = BigInt.apply(words(2))
			}
		} catch {
			case e: Exception => Console.err.println(e.getMessage)
		}
		return (e, s, n)
	}

	def zapisiKljuce(): Unit = {
		try {
			val writer = new FileWriter(keysPath)
			writer.append(e + " ").append(s + " ").append(n.toString)
			writer.close
		} catch {
			case e: Exception => Console.err.println(e.getMessage)
		}
	}

	def zapisiPodatke(podatki: List[BigInt]): Unit = {
		try {
			val writer = new FileWriter(messPath)
			writer.append(podatki.length + " ")
			for (e <- podatki) {
				writer.append(kriptiraj(e) + " ")
			}
			writer.close
		} catch {
			case e: Exception => Console.err.println(e.getMessage)
		}
	}

	def beriPodatke(): List[BigInt] = {
		var list = MutableList[BigInt]()
		var first = true
		for (line <- Source.fromFile(messPath).getLines) {
			for (n <- line.split(" ")) {
				if (!first) list += BigInt.apply(n)
				else first = false
			}
		}
		return dekriptirajPodatke(list.toList)
	}

	def kriptirajPodatke(list: List[BigInt]): List[BigInt] = {
		var listOut = MutableList[BigInt]()
		for (e <- list) {
			listOut += kriptiraj(e)
		}
		return listOut.toList
	}

	def dekriptirajPodatke(list: List[BigInt]): List[BigInt] = {
		var listOut = MutableList[BigInt]()
		for (e <- list) {
			listOut += dekriptiraj(e)
		}
		return listOut.toList
	}

	def dekriptiraj(stevilo: BigInt) = MathBI.modPow(stevilo, s, n)

	def kriptiraj(stevilo: BigInt) = MathBI.modPow(stevilo, e, n)

}
