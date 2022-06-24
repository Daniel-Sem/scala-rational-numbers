package rationals

import scala.annotation.tailrec

object rationalNumbers extends App {

  case class Rational(num: Int, den: Int = 1) {
    try {
      require(den != 0) // precondition the denominator is not zero
    } catch {
      case i: IllegalArgumentException => println(s"The denominator cannot be equal to zero, the $num/0" +
        " rational number will not be created.")
    }
    @tailrec
    private def gcd( a:Int, b: Int): Int =
    // returns the greatest common divisor
      if (b == 0) a else gcd(b, a % b)

    private val g = gcd(num.abs, den.abs)
    val numerator: Int = num / g
    val denominator: Int = den / g

    // def this(num:Int) = this(num,1) // replaced by the default value for the denominator
    // auxiliary constructor for a rational number with a denominator of 1


    override def toString: String = s"$numerator/$denominator"
    // shows the rational

    def + (other: Rational):Rational =
    // returns a new Rational, the sum of this and the other
       Rational(
        numerator * other.denominator + other.numerator * denominator,
        denominator * other.denominator)

    def + (i:Int): Rational =
    // overloaded add method, that takes integer
       Rational(numerator + i * denominator,
                  denominator)

    def - (other:Rational): Rational =
    // returns a new Rational, the subtraction of this and the other
       Rational(numerator * other.denominator - other.numerator * denominator,
                  denominator * other.denominator)

    def - (i:Int):Rational =
       Rational(numerator - i * denominator, denominator)

    def * (other:Rational): Rational =
    // returns a new Rational, the multiplication of this and the other
       Rational(numerator * other.numerator,
                  denominator * other.denominator)

    def * (i:Int): Rational =
    // overloaded multiplication method, that takes integer
       Rational(numerator * i, denominator)

    def / (other:Rational): Rational =
    // returns a new Rational, the division between this and the other
       Rational(numerator * other.denominator, denominator * other.numerator)

    def / (i:Int): Rational =
       Rational(numerator, denominator * i)

    def lessThan(other: Rational): Boolean =
    // checks if this rational is less than a given rational, returns bool
      this.numerator * other.denominator < this.denominator * other.numerator

    def max(other: Rational):Rational =
    // compares this rational and a given rational and returns the bigger one
      if (this.lessThan(other)) this else other

  }

    implicit class IntLike(x: Int) {
    // extension methods on Int that take rational number as parameter and return rational number
      def + (y: Rational): Rational = Rational(x) + y
      def - (y: Rational): Rational = Rational(x) - y
      def * (y: Rational): Rational = Rational(x) * y
      def / (y: Rational): Rational = Rational(x) / y
    }

  /*

  val oneHalf = new Rational(1, 2)
  println(oneHalf)
  val twoThirds = new Rational(2, 3)
  println(twoThirds)

  val notValid = new Rational(5, 0)
  println(notValid)
  val notValid2 = new Rational(23, 0)
  println(notValid2)

  val result = oneHalf + twoThirds
  val result2 = oneHalf * twoThirds
  println(result)
  println(result2)
  println(result.numerator)
  println(result.denominator)
  println(result.lessThan(oneHalf))
  println(oneHalf.max(twoThirds))
  println(result.max(twoThirds))
  println()
  println(result * 2)

  println()
  println(new Rational(2,3) - new Rational(7,8))
  println(new Rational(2,3) - 4)
  println(new Rational(2,3) * new Rational(7,8))
  println(new Rational(2,3) * 4)
  println(new Rational(2,3) / new Rational(7,8))
  println(new Rational(2,3) / 4)

  println()
  println(2 + new Rational(5,3))
  println(4 - new Rational(2,3))
  println(6 * new Rational(7,8))
  println(12 / new Rational(2,3))

  */

}


