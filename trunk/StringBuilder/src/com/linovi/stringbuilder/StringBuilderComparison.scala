package com.linovi.stringbuilder

import java.util.Date

object StringBuilderComparison
{
  def main(args: Array[String])
  {
    val times = 100000
    val testString = "test"
    var s1 = ""
    
    println("Concatenating " + times + " Strings with built-in StringBuilder...")
    val time1 = System.currentTimeMillis()
    s1 = ""
    val sb: scala.collection.mutable.StringBuilder = new scala.collection.mutable.StringBuilder()
    for(i <- 1 to times)
    {
      sb.append(testString)
    }
    val time2 = System.currentTimeMillis()
    println("Done!")
    println("Concatenation with built-in StringBuilder took " + (time2 - time1) + " milliseconds.")
    val time3 = System.currentTimeMillis()
    sb.toString
    val time4 = System.currentTimeMillis()
    println("toString of built-in StringBuilder took " + (time4 - time3) + " milliseconds.")
    println()
    
    val capacity = 64
    println("Concatenating "+ times +" Strings with my StringBuilder(" + capacity + ")...")
    val time5 = System.currentTimeMillis()
    s1 = ""
    val msb: StringBuilder = new StringBuilder(capacity)
    for(i <- 1 to times)
    {
      msb.append(testString)
    }
    val time6 = System.currentTimeMillis()
    println("Done!")
    println("Concatenation with my StringBuilder(" + capacity + ") took " + (time6 - time5) + " milliseconds.")
    val time7 = System.currentTimeMillis()
    var result = msb.toString
    val time8 = System.currentTimeMillis()
    println("toString of my StringBuilder(" + capacity + ") took " + (time8 - time7) + " milliseconds.")
    println()
    
    /*
    println("Concatenating " + times + " Strings with + operator...")
    val time9 = System.currentTimeMillis()
    for(i <- 1 to times)
    {
      s1 = s1 + testString
    }
    val time10 = System.currentTimeMillis()
    println("Done!")
    println("Concatenation with + operator took " + (time10 - time9) + " milliseconds.")
    
    println()
    */
  }
}