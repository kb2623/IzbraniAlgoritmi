/**
  * Created by klemen on 18.10.2016.
  */
object MathK {
   def isWhole(x: BigInt): Boolean = x % 2 == 0

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
}

class PrimeRandomGenerator(
      m: BigInt = MathK.exp(2, 32),
      a: BigInt = 69069,
      b: BigInt = 0,
      rn: BigInt = 1
) extends RandomGenerator(m, a, b, rn) {

   def this() = this(MathK.exp(2, 32), 69069, 0, 1)

   def testNaive(r: BigInt): Boolean = {
      val g = MathK.sqrt(r)
      var j = BigInt(3)
      while (r % j != 0 && j <= g) {
         j += 2
      }
      if (j > g) return true
      else return false
   }

   def naive(): BigInt = {
      var r = lcg()
      if (MathK.isWhole(r)) r += 1
      while (!testNaive(r)) {
         r += 2
      }
      return r
   }

   def milerRabin(s: BigInt = 3): BigInt = {
      var r = lcg()
      if (MathK.isWhole(r)) r += 1
      while (!testMilerRabin(r, s)) {
         if (MathK.isWhole(r)) r += 1
         else r += 2
      }
      return r
   }

   def testMilerRabin(r: BigInt, s: BigInt): Boolean = {
      if (r <= 3)  return true
      if (MathK.isWhole(r)) return false
      var (d, k) = solveEq(r)
      for (j <- BigInt(1) to s) {
         var a = random(2, r - 2)
         var x = MathK.modPow(a, d, r)
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
   private def solveEq(r: BigInt): Tuple2[BigInt, BigInt] = {
      var d = r - 1
      var k = BigInt(0)
      while (!MathK.isWhole(d)) {
         d /= 2
         k += 1
      }
      return (d, k)
   }


   private def loop(r: BigInt, k: BigInt, x1: BigInt): BigInt = {
      var x = x1
      for (i <- BigInt(1) to k - 1) {
         if (x == r - 1) return x
         else x = MathK.modPow(x, 2, r)
      }
      return x
   }
}

