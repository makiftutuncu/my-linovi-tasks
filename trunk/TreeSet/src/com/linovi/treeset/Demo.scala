package com.linovi.treeset

object Demo
{
  def main(args: Array[String]): Unit =
  {
    val tree = ImmutableTreeSet()
    println(tree + "\n")
    
    val tree2 = add(tree, 13)
    val tree3 = add(tree2, 10)
    val tree4 = add(tree3, 14)
    val tree5 = add(tree4, 7)
    val tree6 = add(tree5, 11)
    val tree7 = add(tree6, 15)
    val tree8 = add(tree7, 5)
    val tree9 = add(tree8, 9)
    val tree10 = add(tree9, 12)
    val tree11 = add(tree10, 6)
    val tree12 = remove(tree11, 6)
    val tree13 = remove(tree12, 11)
    val tree14 = remove(tree13, 10)
    val tree15 = remove(tree14, 13)
    
    contains(tree15, 7)
    
    filter(tree11, isEven)
    
    filter(tree11, isOdd)
    
    map(tree11, takeSquares)
  }
  
  def add(t: ImmutableTreeSet, v: Int): ImmutableTreeSet =
  {
    val newTree = t + v
    println("After adding " + v)
    println(newTree + "\n")
    
    newTree
  }
  
  def remove(t: ImmutableTreeSet, v: Int): ImmutableTreeSet =
  {
    val newTree = t - v
    println("After removing " + v)
    println(newTree + "\n")
    
    newTree
  }
  
  def contains(t: ImmutableTreeSet, v: Int): Unit = println("Tree contains " + v + ": " + t(v) + "\n")
  
  def filter(t: ImmutableTreeSet, f: (Int) => Boolean): Unit = println("Filtered tree\n" + t.filter(f) + "\n")
  
  def map(t: ImmutableTreeSet, f: (ImmutableNode) => ImmutableNode): Unit = println("Mapped tree\n" + t.map(f) + "\n")
  
  def isEven(v: Int): Boolean = v % 2 == 0
  
  def isOdd(v: Int): Boolean = !isEven(v)
  
  def takeSquares(n: ImmutableNode): ImmutableNode = ImmutableNode(n.value * n.value, n.left, n.right)
}