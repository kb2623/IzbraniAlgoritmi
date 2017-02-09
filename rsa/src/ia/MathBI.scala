package ia

object MathBI {
	def isWhole(x: BigInt): Boolean = x % 2 == 0

	def abs(x: BigInt): BigInt = if (x < 0) return -x else return x

	def exp(a: BigInt, b: BigInt): BigInt = {
		var res = BigInt(1)
		var base = a
		var e = b
		while (true) {
			if ((e & 1) == 1) res *= base
			e >>= 1
			if (e == 0) return res
			base *= base
		}
		return res
	}

	def sqrt(number : BigInt): BigInt = {
		def next(n : BigInt, i : BigInt) : BigInt = (n + i/n) >> 1
		val one = BigInt(1)
		var n = one
		var n1 = next(n, number)
		while ((n1 - n).abs > one) {
			n = n1
			n1 = next(n, number)
		}
		while (n1 * n1 > number) {
			n1 -= one
		}
		return n1
	}

	def modPow(num: BigInt, powa: BigInt, mod: BigInt): BigInt = {
		var pow = powa
		var test = BigInt(1)
		var n = num
		while (pow > 0) {
			if ((pow & 1) == 1) test = ((test % mod) * (n % mod)) % mod
			n = ((n % mod) * (n % mod)) % mod
			pow >>= 1
		}
		return test
	}

	def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) return a else gcd(b, a % b)

	def modLinearEqu(a: BigInt, b: BigInt, n: BigInt): Any = {
		var (d, x, y)  = extendedEuclid(a, n);
		if (d % b == 0) {
			val x0 = x * ((b / d) % n)
			if (x0 < 0) return x0 + n
			else return x0
		} else {
			return null
		}
	}

	def extendedEuclid(a: BigInt, b: BigInt): (BigInt, BigInt, BigInt)  = {
		if (b == 0) {
			return (a, 1, 0)
		} else {
			val (nd, nx, ny) = extendedEuclid(b, a % b)
			return (nd, ny, nx - (a / b) * ny)
		}
	}

	def extendedEuclidI(ai: BigInt, bi: BigInt): (BigInt, BigInt, BigInt) = {
		var (a, b) = (ai, bi)
		var (x, lastx, y, lasty) = (BigInt(0), BigInt(1), BigInt(1), BigInt(0))
		while (b != 0) {
			val quotient = a / b
			var temp = b
			b = a % b
			a = temp
			temp = x
			x = lastx - quotient * x
			lastx = temp
			temp = y
			y = lasty - quotient * y
			lasty = temp
		}
		return (a, lastx, lasty)
	}
}
