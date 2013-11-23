package com.linovi.learningclasses

import scala.collection.mutable.Map

/** This is a normal class and it is called the companion class of
 * ChecksumAccumulator object. This should have the same name and be in the same
 * source file as the object */
class ChecksumAccumulator
{
  private var sum = 0;
  
  /* If return type is Unit, which means this method is used for it's side
   * effects, it can be declared like this (no result type, = and between { } */
  def add(b : Byte) { sum += b }

  // Scala returns the last computed value, no need for an explicit return
  def checksum() : Int = ~(sum & 0xFF) + 1
}

/** This is a singleton and it is called the companion object of
 * ChecksumAccumulator class. This should have the same name and be in the same
 * source file as the class */
object ChecksumAccumulator
{
  // Cache to keep previous checksum calculations
  private val cache = Map[String, Int]()
  
  def calculate(string : String) : Int =
  {
    if(cache.contains(string)) // If s exists in the cache
      cache(string) // Get that value and return (again no explicit return here)
    else
    {
      val accumulator = new ChecksumAccumulator // Create a new accumulator
      
      for(character <- string) // For every character in the string
        accumulator.add(character.toByte) // Add it as the character to the accumulator
      
      val checksum = accumulator.checksum() // Calculate the checksum
      cache += (string -> checksum) // Add the result to the cache
      
      checksum // Return resulting checksum
    }
  }
  
  // Main method to start the application
  def main(args : Array[String])
  {
    val message = "Hello world!";
    println("Message is: " + message)
    println("Checksum is: " + ChecksumAccumulator.calculate(message))
  }
}