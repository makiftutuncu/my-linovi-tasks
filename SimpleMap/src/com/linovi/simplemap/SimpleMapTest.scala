package com.linovi.simplemap

/**
 * An example scenario to test the behavior of {@link SimpleMap} implementation
 */
object SimpleMapTest
{
  def main(args: Array[String]): Unit =
  {
    val map = new SimpleMap
    
    println("Current map:")
    println(map)
    println("Is map empty: " + map.isEmpty)
    
    println("Putting new pairs...")
    
    map ++ ("Ezgi", "Akif")
    map ++ ("Fatih", "Taner")
    map ++ ("Okan", "Emre")
    map ++ ("Muhittin Topalak", "Kezban")
    map ++ ("qwe", "q")
    map ++ ("asd", "w")
    map ++ ("zxc", "e")
    map ++ ("vbn", "r")
    map ++ ("fgh", "t")
    map ++ ("rty", "y")
    map ++ ("ıuo", "u")
    map ++ ("jkl", "ı")
    map ++ ("lopğ", "o")
    map ++ ("ömç", "p")
    
    println("Current map:")
    println(map)
    println("Size of the map: " + map.size)
    
    println("Map contains a pair with key \"Ezgi\": " + map.contains("Ezgi"))
    println("Value of the pair with key \"Ezgi\": " + map("Ezgi"))
    
    println("Deleting the pair with key \"Okan\"...")
    
    map -- "Okan"
    
    println("Current map:")
    println(map)
    println("Size of the map: " + map.size)
  }
}