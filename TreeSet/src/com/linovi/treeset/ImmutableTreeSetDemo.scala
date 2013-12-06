package com.linovi.treeset

object ImmutableTreeSetDemo
{
  def main(args: Array[String]): Unit =
  {
    val tree = ImmutableTreeSet()
    val tree2 = tree + 13 + 10 + 14 + 7 + 11 + 15 + 5 + 9 + 12 + 6
    
    val tree3 = tree2 - 7
    println()
  }
}