package com.linovi.simplemap

/**
 * A simple implementation of a String to String map
 */
class SimpleMap
{
  /** Buffer in which the map items will be stored */
  private val buffer = new LinkedList
  
  /** Gets the value with the given key
   *  
   *  @return Value with the given key or null if the given key doesn't exist
   *  in the map */
  def apply(k: String): String = get(k)
  
  /** Gets the value with the given key
   *  
   *  @return Value with the given key or null if the given key doesn't exist
   *  in the map */
  def get(k: String): String =
  {
    val node: Node = buffer(k)
    if(node != null) node.value else null
  }
  
  /** Puts the given key-value pair to the map or updates the value of the pair
   *  if there is already a pair with the given key in the map
   *  
   *  @return true if the given pair is added as a new pair, false if the map
   *  already contains a pair with the given key and value is updated */
  def ++(k: String, v: String): Boolean = put(k, v)
  
  /** Puts the given key-value pair to the map or updates the value of the pair
   *  if there is already a pair with the given key in the map
   *  
   *  @return true if the given pair is added as a new pair, false if the map
   *  already contains a pair with the given key and value is updated */
  def put(k: String, v: String): Boolean =
  {
    val node: Node = buffer(k)
    if(node == null)
    {
      buffer ++ new Node(k, v)
      
      true
    }
    else
    {
      node.setValue(v)
      
      false
    }
  }
  
  /** Deletes the pair with given key
   *  
   *  @return true if the given key exist in the list and pair is successfully
   *  deleted, false otherwise */
  def --(k: String): Boolean = buffer -- k
  
  /** Deletes the pair with given key
   *  
   *  @return true if the given key exist in the list and pair is successfully
   *  deleted, false otherwise */
  def delete(k: String): Boolean = buffer -- k
  
  /** Checks if the map is empty
   * 
   *  @return true if the map is empty, false otherwise */
  def isEmpty: Boolean = buffer.count == 0
  
  /** Checks if the map contains a pair with the given key
   * 
   *  @return true if the map is contains a pair with the given key, false
   *  otherwise */
  def contains(k: String): Boolean = buffer(k) != null
  
  /** Gets the number of pairs in the map
   * 
   *  @return Number of pairs in the map */
  def size: Int = buffer.count
  
  /** Gives a String representation of the map */
  override def toString =
  {
    val builder = new StringBuilder("{")
    
    // Traverse the buffer and add each pair
    var node = buffer.first
    while(node != null)
    {
      builder.append(node.toString)
      builder.append(",")
      node = node.next
    }
    
    builder.dropRight(1).append("}").toString
  }
}