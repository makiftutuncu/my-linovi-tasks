package com.linovi.stringbuilder

/**
 * A simple node for the linked list having an array of characters with
 * specified length
 */
class Node(c: Int)
{
  var data: Array[Char] = new Array[Char](c)
  var next: Node = null
  var length = 0
  
  override def toString = new String(data)
}

/**
 * A simple linked list implementation for the StringBuilder
 */
class LinkedList
{
  var first: Node = null
  var last: Node = null
  var count: Int = 0
  
  def add(node: Node): Unit =
  {
    if(count <= 0)
    {
      first = node
      last = node
    }
    else
    {
      last.next = node
      last = node
    }
    count += 1
  }
}

/**
 * A custom implementation of StringBuilder for efficient and mutable String
 * concatenation
 */
class StringBuilder
{
  private val buffer: LinkedList = new LinkedList
  var length = 0
  var capacity = 16
  
  // Auxiliary constructors
  def this(c: Int) = {this(); if(c > 0) capacity = c}
  def this(s: String) = {this(); append(s)}
  
  /**
   * Creates nodes from the given String each having a character array with
   * length between 0 and capacity
   */
  private def createNodes(s: String): Array[Node] =
  {
    // If given String is not null
    if(s != null)
    {
      // Calculate how many nodes will be needed
      val nodeCount: Int = (s.length() / capacity) + 1
      // Create array of nodes
      val nodes = new Array[Node](nodeCount)
      
      // If given String fits in a single node
      if(s.length() < capacity)
      {
        // Create a node for given String
        nodes(0) = new Node(capacity)
        val currentChars = s.toCharArray()
        Array.copy(currentChars, 0, nodes(0).data, 0, currentChars.length)
        nodes(0).length += currentChars.length
      }
      else
      {
        // For each node
        for(i <- 0 until nodeCount)
        {
          // Create node
          nodes(i) = new Node(capacity)
          
          // Get the substring starting with the contents of the current node 
          val currentChars = s.substring(i * capacity).toCharArray()
          
          // If the remaining is less than capacity
          if(currentChars.length < capacity)
          {
            // Copy as the length of the current characters
            Array.copy(currentChars, 0, nodes(i).data, 0, currentChars.length)
          }
          else
          {
            // Copy as the length of the capacity
            Array.copy(currentChars, 0, nodes(i).data, 0, capacity)
          }
          
          nodes(i).length += currentChars.length
        }
      }
      
      // Return array of nodes
      nodes
    }
    else
    {
      // Given String was null so return null
      null
    }
  }
  
  /**
   * Appends the given String to the end of characters in this StringBuilder
   */
  def append(s: String): StringBuilder =
  {
    if(s != null)
    {
      // If buffer is empty
      if(buffer.count == 0)
      {
        // Buffer is empty, create nodes and add all generated nodes to the buffer
        for(node <- createNodes(s)) buffer.add(node)
      }
      else
      {
        // String fits to the empty space in the last node
        if(s.length() <= capacity - buffer.last.length)
        {
          // Copy the String into the empty space in the last node
          Array.copy(s.toCharArray(), 0, buffer.last.data, buffer.last.length, s.length())
          
          buffer.last.length += s.length()
        }
        else
        {
          // Nodes to be created and added
          var nodes: Array[Node] = null
        
          // If there is any empty space in the last node
          if(buffer.last.length < capacity)
          {
            // Calculate remaining empty space in the last node
            val remainingSpace = capacity - buffer.last.length
          
            // Get the characters that would fit into the empty space in the last node
            val charsToCutFromBeginning: Array[Char] = s.substring(0, remainingSpace).toCharArray()
          
            // Copy characters into the empty space in the last node
            Array.copy(charsToCutFromBeginning, 0, buffer.last.data, buffer.last.length, remainingSpace)
            
            buffer.last.length += remainingSpace
          
            // Create nodes with the remaining part of the String
            nodes = createNodes(s.substring(remainingSpace))
          }
          else
          {
            // No empty space in the last node so just create a new nodes
            nodes = createNodes(s)
          }
        
          // Add all generated nodes to the buffer
          for(node <- nodes) buffer.add(node)
        }
      }
      
      // Increase the length
      length += s.length()
    }
    
    // Return this StringBuilder to enable chained calls
    this
  }
  
  /**
   * Appends the String representation of given value to the end of characters
   * in this StringBuilder
   */
  def append(b: Boolean): StringBuilder = append(String.valueOf(b))
  def append(b: Byte): StringBuilder = append(String.valueOf(b))
  def append(c: Char): StringBuilder = append(String.valueOf(c))
  def append(i: Int): StringBuilder = append(String.valueOf(i))
  def append(l: Long): StringBuilder = append(String.valueOf(l))
  def append(f: Float): StringBuilder = append(String.valueOf(f))
  def append(d: Double): StringBuilder = append(String.valueOf(d))
  def append(arr: Array[Char]): StringBuilder = append(arr.mkString)
  def append(a: Any): StringBuilder = append(String.valueOf(a))
  
  /**
   * Returns a String containing all the characters in this StringBuilder
   */
  override def toString =
  {
    val result = new Array[Char](length)
    var n = buffer.first
    var last = 0
    while(n != null)
    {
      val copyLength = n.data.length
      Array.copy(n.data, 0, result, last, copyLength)
      last += copyLength
      n = n.next
    }
    new String(result)
  }
}