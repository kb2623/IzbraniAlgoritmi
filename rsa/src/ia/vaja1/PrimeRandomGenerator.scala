package ia.vaja1

import scala.util.Random
import ia.MathBI

class PrimeRandomGenerator (
		m: BigInt = MathBI.exp(2, 32),
		a: BigInt = 69069,
		b: BigInt = 0,
		rn: BigInt = 1
) extends RandomGenerator(m, a, b, rn) {

	def this() = this(MathBI.exp(2, 128), MathBI.abs(new Random().nextLong()), 0, 1)

	def testNaive(r: BigInt): Boolean = {
		val g = MathBI.sqrt(r)
		var j = BigInt(3)
		while (r % j != 0 && j <= g) {
			j += 2
		}
		if (j > g) return true
		else return false
	}

	def naive(): BigInt = {
		var r = lcg()
		if (MathBI.isWhole(r)) r += 1
		while (!testNaive(r)) {
			r += 2
		}
		return r
	}

	def milerRabin(): BigInt = {
		return milerRabin(4)
	}

	def milerRabin(s: BigInt = 3): BigInt = {
		var r = lcg()
		if (MathBI.isWhole(r)) r += 1
		while (!testMilerRabin(r, s)) {
			if (MathBI.isWhole(r)) r += 1
			else r += 2
		}
		return r
	}

	def testMilerRabin(r: BigInt, s: BigInt): Boolean = {
		if (r <= 3)  return true
		if (MathBI.isWhole(r)) return false
		var (d, k) = solveEq(r)
		for (j <- BigInt(1) to s) {
			var a = random(2, r - 2)
			var x = MathBI.modPow(a, d, r)
			if (x != 1) {
				x = loop(r, k, x)
				if (x != r - 1) {
					return false
				}
			}
		}
		return true
	}

	/**
	  * Metoda za resevanje enacbe: d * 2^k = r - 1
	  * @param r Random number
	  * @return k -> resen, d -> resen
	  */
	private def solveEq(r: BigInt): (BigInt, BigInt) = {
		var d = r - 1
		var k = BigInt(0)
		while (!MathBI.isWhole(d)) {
			d /= 2
			k += 1
		}
		return (d, k)
	}

	private def loop(r: BigInt, k: BigInt, x1: BigInt): BigInt = {
		var x = x1
		for (i <- BigInt(1) to k - 1) {
			if (x == r - 1) return x
			else x = MathBI.modPow(x, 2, r)
		}
		return x
	}
}

