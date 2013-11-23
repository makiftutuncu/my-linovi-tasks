package com.linovi.rationalnumbers

class Rational(n : Int, d : Int) extends Ordered[Rational] // n and d are class parameters
{
  /* This is inside the primary constructor because class have parameters and
   * this expression is not in any method or field declaration
   * println("Created a rational " + n + "/" + d) */
  
  // Fields
  
  // Gcd of n and d to (absolute values of them) normalize the rational number
  private val g = gcd(n.abs, d.abs)
  val numerator = n / g
  val denominator = d / g
  
  // Create a constraint on the class parameters that denominator shouldn't be 0
  require(d != 0)
  
  // Auxiliary constructor
  def this(n : Int) = this(n, 1)
  
  // Add a rational number to this one and return a new instance of Rational
  def +(that : Rational) : Rational =
    new Rational(	numerator * that.denominator + that.numerator * denominator,
    				denominator * that.denominator)
  
  // Add an integer this rational number and return a new instance of Rational
  def +(i : Int) : Rational = new Rational(numerator + i * denominator, denominator)
  
  // Subtract a rational number from this one and return a new instance of Rational
  def -(that : Rational) : Rational = this + new Rational(-that.numerator, that.denominator)
  
  // Subtract an integer from this rational number and return a new instance of Rational
  def -(i : Int) : Rational = this + (-i)
  
  // Multiply this rational number with another one and return a new instance of Rational
  def *(that : Rational) : Rational =
    new Rational(numerator * that.numerator, denominator * that.denominator)
  
  // Multiply this rational number by an integer and return a new instance of Rational
  def *(i : Int) : Rational = new Rational(numerator * i, denominator)
  
  // Divide this rational number by another one and return a new instance of Rational
  def /(that : Rational): Rational = this * new Rational(that.denominator, that.numerator)
  
  // Divide this rational number by an integer and return a new instance of Rational
  def /(i : Int): Rational = this * new Rational(1, i)
  
  // Checks if this rational number is less than the given one
  def lessThan(that : Rational) =
    this.numerator * that.denominator < that.numerator * this.denominator
  
  // Finds the maximum rational number, either this or that
  def max(that : Rational) =
    if(this.lessThan(that)) that else this
  
  // Finds greatest common divisor of given numbers recursively
  private def gcd(a : Int, b : Int) : Int = if(b == 0) a else gcd(b, a % b)
  
  // Compares this rational number to the given rational number
  override def compare(that: Rational) =
  {
    (this.numerator * that.denominator) - (that.numerator * this.denominator)
  }
  
  // Overriding toString
  override def toString = n + "/" + d
}

object Test
{
  def main(args: Array[String])
  {
    val r1 = new Rational(24, 42)
    val r2 = new Rational(3, 5)
    val i1 = 4
    val i2 = 53
    
    println(r1)
    println(r2)
    println(r1 + r2)
    println(r1 - r2)
    println(r1 * r2)
    println(r1 / r2)
    println(r1 + i1)
    println(r1 - i1)
    println(r1 * i1)
    println(r1 / i1)
  }
}