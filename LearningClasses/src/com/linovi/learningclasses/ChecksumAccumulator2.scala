package com.linovi.learningclasses

/** This object extends Application trait and can be run as an application. Note
 *  that is doesn't have a main method. Instead, the code inside the block is
 *  used as the main method. */
object ChecksumAccumulator2 extends Application
{
  val message = "Hello world!";
  println("Message is: " + message)
  println("Checksum is: " + ChecksumAccumulator.calculate(message))
}