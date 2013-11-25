package com.linovi.simplemap

import scala.collection.mutable.HashMap

/**
 * A comparison scenario to test the performance of {@link SimpleMap}
 * implementation against {@link scala.collection.mutable.Map}
 */
object SimpleMapComparison
{
  def main(args: Array[String]): Unit =
  {
    val key = "key"
    val value = "value"
    val times = 100000
    
    val map = new HashMap[String, String]
    val time1 = System.currentTimeMillis()
    for(i <- 1 to times)
      map += (key + i) -> (value + i)
    val time2 = System.currentTimeMillis()
    println("Putting " + times + " pairs with built-in HashMap took " + (time2 - time1) + " milliseconds.")
    println()
    
    val simpleMap = new SimpleMap
    val time3 = System.currentTimeMillis()
    for(i <- 1 to times)
      simpleMap ++ ((key + i), (value + i))
    val time4 = System.currentTimeMillis()
    println("Putting " + times + " pairs with SimpleMap took " + (time4 - time3) + " milliseconds.")
    println()
    
    val time5 = System.currentTimeMillis()
    map.get("key25000")
    val time6 = System.currentTimeMillis()
    println("Accessing a pair with built-in HashMap took " + (time6 - time5) + " milliseconds.")
    println()
    
    val time7 = System.currentTimeMillis()
    simpleMap.get("key25000")
    val time8 = System.currentTimeMillis()
    println("Accessing a pair with SimpleMap took " + (time8 - time7) + " milliseconds.")
    println()
    
    val time9 = System.currentTimeMillis()
    map.remove("key25000")
    val time10 = System.currentTimeMillis()
    println("Removing a pair with built-in HashMap took " + (time10 - time9) + " milliseconds.")
    println()
    
    val time11 = System.currentTimeMillis()
    simpleMap.delete("key25000")
    val time12 = System.currentTimeMillis()
    println("Removing a pair with SimpleMap took " + (time12 - time11) + " milliseconds.")
    println()
  }
}