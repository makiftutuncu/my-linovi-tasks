package com.linovi.functions

object Functions
{
  /** These functions is for testing */
  def equals(a: Int, b: Int): Boolean = a == b
  def multiply(a: Int, b: Int): Int = a * b
  
  /** This function takes
   *    - a value as the first parameter of Type1
   *    - a function taking two parameters of types Type1 and Type2 and
   *      returning a value of Type3
   *  and returns
   *    a function taking a parameter of Type2 and returning Type3 on which the
   *    given parameter of Type1 is partially applied */
  def partial[Type1, Type2, Type3](a: Type1, f: (Type1, Type2) => Type3): Type2 => Type3 = (x: Type2) => f(a, x)
  
  /** This function takes
   *    a function taking two parameters of types Type1 and Type2 and returning
   *    a value of Type3
   *  and returns
   *    a function taking a parameter of Type1 and returning a function taking
   *    a parameter of Type2 and returning a value of Type3 on which the given
   *    parameter of Type1 is partially applied */
  def curry[Type1, Type2, Type3](f: (Type1, Type2) => Type3): Type1 => (Type2 => Type3) = (x: Type1) => (y: Type2) => f(x, y)
  
  /** This function takes
   *    a function taking a parameter of type Type1 and returning a function
   *    taking a parameter of Type1 and returning a function taking a parameter
   *    of Type2 and returning a value of Type3
   *  and returns
   *    a function taking two parameters of types Type1 and Type2 and returning
   *    a value of Type3 which is calculated by partially applying given
   *    parameters of Type1 and Type2 */
  def uncurry[Type1, Type2, Type3](f: Type1 => (Type2 => Type3)): (Type1, Type2) => Type3 = (x: Type1, y: Type2) => f(x)(y)
  
  /** This function takes
   *    - a function taking a parameter of type Type2 and returning a value of
   *    Type3
   *    - a function taking a parameter of type Type1 and returning a value of
   *    Type2
   *  and returns
   *    a function taking a parameter of type Type1 and returning a value of
   *    Type3 which is calculated by composing the given functions as parameters */
  def compose[Type1, Type2, Type3](f: Type2 => Type3, g: Type1 => Type2): Type1 => Type3 = (x: Type1) => f(g(x))
  
  def main(args: Array[String]): Unit =
  {
    // Prints true
    val isOne = partial[Int, Int, Boolean](1, equals)
    println(isOne(1))
    
    // Prints 10
    val multiplyInt = curry[Int, Int, Int](multiply)
    val twice = multiplyInt(2)
    println(twice(5))
    
    // Prints 6
    val multiplication = uncurry[Int, Int, Int](multiplyInt)
    println(multiplication(2, 3))
    
    // Prints 18
    val triple = multiplyInt(3)
    val sixTimes = compose[Int, Int, Int](twice, triple)
    println(sixTimes(3))
    
  }
}