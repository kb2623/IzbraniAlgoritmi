package ia.vaja1

class RandomGenerator(
		var m: BigInt = Math.pow(2, 32).toLong,
		val a: BigInt = 69069,
		val b: BigInt = 0,
		var rn: BigInt = 1
) {
	def this() = this(Math.pow(2, 32).toLong, 69069, 0, 1)

	def lcg(): BigInt = {
		rn = (a * rn + b) % m
		if (rn == 0) {
			rn = 1
			return 0
		}
		return rn
	}

	def random(a: BigInt, b: BigInt): BigInt = (a + lcg()) % (b - a + 1)
}
