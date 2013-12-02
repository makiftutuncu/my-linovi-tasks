package com.linovi.treeset

/** Represents a node in the TreeSet */
class Node(var value: Int)
{
  var left: Node = _
  var right: Node = _
  
  override def toString = value.toString
}

/** A simple implementation of a binary tree */
class TreeSet
{
  /** Starting node of the tree */
  private var root: Node = _
  /** Number of nodes in the tree */
  private var count: Int = 0
  
  def +(v: Int) = add(v)
  def -(v: Int) = remove(v)
  def apply(v: Int) = contains(v)
  def size = count
  
  /** Adds the given value as a new node to the tree */
  def add(v: Int): TreeSet =
  {
    if(count == 0)
    {
      root = new Node(v)
      count += 1
    }
    else
    {
      findFreeNode(root, v) match
      {
        case Some(n) =>
        {
          if(v < n.value)
            n.left = new Node(v)
          else
            n.right = new Node(v)
          
          count += 1
        }
        
        case None => None
      }
    }
    
    this
  }
    
  /** Removes the given value if it exists in the tree */
  def remove(v: Int): Boolean =
  {
    if(count == 0)
      false
    else
      removeValue(root, root, v)
  }
  
  /** Checks whether the given value exists in the tree */
  def contains(v: Int): Boolean = containsValue(root, v)
  
  /** Recursively finds the correct place to add the given value */
  private def findFreeNode(n: Node, v: Int): Option[Node] =
  {
    if(n == null)
      None
    else if(v == n.value)
      None
    else if(v < n.value)
    {
      if(n.left == null)
        Option(n)
      else
        findFreeNode(n.left, v)
    }
    else
    {
      if(n.right == null)
        Option(n)
      else
        findFreeNode(n.right, v)
    }
  }
  
  /** Traverses the tree starting from a node and checks if the given value
   *  exists in the tree */
  private def containsValue(n: Node, v: Int): Boolean =
  {
    if(n == null)
      false
    else if(v == n.value)
      true
    else if(v < n.value)
      containsValue(n.left, v)
    else
      containsValue(n.right, v)
  }
  
  /** Recursively adds the given node and all it's sub-nodes to the tree */
  private def addNode(n: Node): Unit =
  {
    if(n != null)
    {
      findFreeNode(root, n.value) match
      {
        case Some(node) =>
        {
          if(n.value < node.value)
            node.left = n
          else
            node.right = n
        }
        
        case None => None
      }
    }
  }
  
  /** Traverses the tree starting from a node with it's parent and deletes the
   *  given value if it exists in the tree */
  private def removeValue(n: Node, p: Node, v: Int): Boolean =
  {
    if(n == null)
      false
    else if(v < n.value)
      removeValue(n.left, n, v)
    else if(v > n.value)
      removeValue(n.right, n, v)
    else
    {
      val left = n.left
      val right = n.right
      
      if(v == root.value)
      {
        // This was root node
        
        root.left = null
        root.right = null
        
        if(left != null)
        {
          root = left
          
          if(right != null)
            addNode(right)
        }
        else
        {
          root = right
          
          if(left != null)
            addNode(left)
        }
      }
      else
      {
        if(v < p.value)
          p.left = null
        else
          p.right = null
        
        if(left != null)
          addNode(left)
        if(right != null)
          addNode(right)
      }
      
      count -= 1
      
      true
    }
  }
  
  /** Traverses the tree starting from a node and prints it's contents */
  private def print(n: Node): String =
  {
    if(n == null)
      "_"
    else
      "{" + n + ", " + print(n.left) + ", " + print(n.right) + "}"
  }
  
  override def toString = "Size: " + count + "\n" + print(root)
}

object TreeSetTest
{
  val tree = new TreeSet
  
  def add(v: Int): Unit =
  {
    println("Adding " + v + "...")
    tree + v
    println(tree + "\n")
  }
  
  def remove(v: Int): Unit =
  {
    println("Delete " + v + ": " + (tree - v))
    println(tree + "\n")
  }
  
  def main(args: Array[String]): Unit =
  {
    println(tree + "\n")
    
    remove(5)
    
    add(8)
    add(9)
    add(4)
    add(3)
    add(7)
    add(6)
    add(5)
    
    add(8)
    
    println("Tree contains 5: " + tree(5) + "\n")
    
    remove(4)
    remove(3)
    remove(8)
    
    remove(2)
  }
}