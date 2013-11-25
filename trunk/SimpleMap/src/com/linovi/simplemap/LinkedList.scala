package com.linovi.simplemap

/**
 * A simple implementation of a linked list to provide background to
 * {@link SimpleMap}
 */
class LinkedList
{
  /** First node of the list */
  var first: Node = _
  /** Last node of the list */
  var last: Node = _
  /** Number of nodes in the list */
  var count: Int = 0
  
  /** Adds a given node to the end of the list
   *  
   *  @return true if successfully added, false otherwise */
  def ++(node: Node): Unit = add(node)
  
  /** Adds a given node to the end of the list
   *  
   *  @return true if successfully added, false otherwise */
  def add(node: Node): Unit =
  {
    if(node != null)
    {
      if(count <= 0)
      {
        first = node
        last = node
        node.previous = null
        node.next = null
      }
      else
      {
        node.previous = last
        last.next = node
        last = node
      }
      count += 1
    }
  }
  
  /** Gets the value with the given key
   *  
   *  @return Value with the given key or null if the given key doesn't exist
   *  in the list */
  def apply(key: String): Node = get(key)
  
  /** Gets the value with the given key
   *  
   *  @return Value with the given key or null if the given key doesn't exist
   *  in the list */
  def get(key: String): Node =
  {
    if(key == null)
      null
    else
    {
      var node = first
      var isFound = false
      while(!isFound && node != null)
      {
        if(node.key == key)
          isFound = true
        else
          node = node.next
      }
      node
    }
  }
  
  /** Deletes the node with given key
   *  
   *  @return true if the given key exist in the list and node is successfully
   *  deleted, false otherwise */
  def --(key: String): Boolean = delete(key)
  
  /** Deletes the node with given key
   *  
   *  @return true if the given key exist in the list and node is successfully
   *  deleted, false otherwise */
  def delete(key: String): Boolean =
  {
    if(key == null)
      false
    else
    {
      val nodeToDelete = get(key)
      
      if(nodeToDelete == null)
        false
      else
      {
        val prevNode = nodeToDelete.previous
        prevNode.next = nodeToDelete.next
        count -= 1
        true
      }
    }
  }
}