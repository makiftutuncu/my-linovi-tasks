package com.linovi.simplemap

/**
 * An example scenario to test the behavior of {@link SimpleMap} implementation
 */
object SimpleMapTest
{
  def main(args: Array[String]): Unit =
  {
    val a = new LinkedList
    a.add(new Node("Ezgi", "Akif"))
    a.add(new Node("Fatih", "Taner"))
    a.add(new Node("Okan", "Ozan"))
    a.add(new Node("Emre", "Cem"))
    a.add(new Node("Umut", "Muhittin Topalak"))
    val b = a.get("Emre")
    println(b)
    println(a.delete("Umut"))
  }
}