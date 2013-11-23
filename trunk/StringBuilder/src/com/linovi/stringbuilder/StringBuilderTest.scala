package com.linovi.stringbuilder

import java.util.Date

object StringBuilderTest
{
  def main(args: Array[String])
  {
    val sb1 = new StringBuilder
    
    println(sb1.toString)
    println(sb1.length)
    println()
    
    sb1.append("Hello")
    println(sb1.toString)
    println(sb1.length)
    println()
    
    sb1.append("World")
    println(sb1.toString)
    println(sb1.length)
    println()
    
    sb1.append("Akif")
    println(sb1.toString)
    println(sb1.length)
    println()
    
    val time1 = System.currentTimeMillis()
    sb1.toString
    val time2 = System.currentTimeMillis()
    println("toString of my StringBuilder took " + (time2 - time1) + " milliseconds.")
    
    val sb2 = new StringBuilder(3)
    println(sb2.capacity)
    val sb3 = new StringBuilder(-1)
    println(sb3.capacity)
    val sb4 = new StringBuilder("Hello")
    println(sb4)
  }
}