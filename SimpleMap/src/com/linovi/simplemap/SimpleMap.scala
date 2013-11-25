package com.linovi.simplemap

/**
 * A simple implementation of a String to String map
 */
class SimpleMap
{
  /** Buffer size */
  private val bufferSize = 4096
  
  /** Buffer in which pairs of the map will be stored */
  private val buffer = new Array[Pair](bufferSize)
  
  /** Number of pairs in the map */
  private var count = 0
  
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
    if(k == null)
    {
      // If key is null, just return null
      null
    }
    else
    {
      // Get the hash code of the given key
      val hash = k.hashCode().abs
      val hashedIndex = hash % bufferSize
      
      // Get the pair at the hashed index
      var pair = buffer(hashedIndex)
      
      // There is no pair found at hashed index, return null
      if(pair == null)
      {
        null
      }
      // There is at least one pair found at hashed index
      else
      {
        // Traverse and find the pair with the key we want
        while(pair != null && pair.key != k)
        {
          pair = pair.next
        }
       
        // If a pair is found, return it's value, otherwise return null
        if(pair != null) pair.value else null
      }
    }
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
    if(k == null)
    {
      // If key is null, just return false
      false
    }
    else
    {
      // Increase the count
      count += 1
      
      // Get the hash code of the given key
      val hash = k.hashCode().abs
      val hashedIndex = hash % bufferSize
      
      // Get the pair at the hashed index
      var pair = buffer(hashedIndex)
      
      // There is no pair found at hashed index
      if(pair == null)
      {
        // Just put the new pair there
        buffer(hashedIndex) = new Pair(k, v)
        
        // Return true
        true
      }
      // There is at least one pair found at hashed index
      else
      {
        /* Traverse to the last pair at this index while checking if the key of
         * the current pair is the one we want */
        while(pair.key != k && pair.next != null)
        {
          pair = pair.next
        }
        
        // We found a pair with the given key
        if(pair.next != null)
        {
          // Update the value of the current pair
          pair.value = v
          
          // Return false because we did an update
          false
        }
        // We traversed to the end of the linked list, no pair with the given key is found
        else
        {
          // Put the new pair at the end of the linked list at this index
          val newPair = new Pair(k, v)
          newPair.previous = pair
          pair.next = newPair
          
          // Return true because we added a new pair
          true
        }
      }
    }
  }
  
  /** Deletes the pair with given key
   *  
   *  @return true if the given key exist in the list and pair is successfully
   *  deleted, false otherwise */
  def --(k: String): Boolean = delete(k)
  
  /** Deletes the pair with given key
   *  
   *  @return true if the given key exist in the list and pair is successfully
   *  deleted, false otherwise */
  def delete(k: String): Boolean =
  {
    if(k == null)
    {
      // If key is null, just return false
      false
    }
    else
    {
      // Get the hash code of the given key
      val hash = k.hashCode().abs
      val hashedIndex = hash % bufferSize
      
      // Get the pair at the hashed index
      var pair = buffer(hashedIndex)
      
      // There is no pair found at hashed index
      if(pair == null)
      {
        // Return false
        false
      }
      // There is at least one pair found at hashed index
      else
      {
        // Traverse and find the pair with the key we want
        while(pair != null && pair.key != k)
        {
          pair = pair.next
        }
       
        // If the pair with the given key is found
        if(pair != null)
        {
          // Decrease the count
          count -= 1
          
          // Decide what to do to delete the pair
          if(pair.next == null && pair.previous == null)
          {
            // Current pair is the only pair in the linked list at hashed index
            buffer(hashedIndex) = null
          }
          else if(pair.next == null && pair.previous != null)
          {
            // Current pair is the last pair in the linked list at hashed index
            pair.previous.next = null
            pair.previous = null
          }
          else if(pair.next != null && pair.previous == null)
          {
            // Current pair is the first pair in the linked list at hashed index
            buffer(hashedIndex) = pair.next
            pair.next = null
          }
          else
          {
            // Current pair is somewhere in the middle of the linked list at hashed index
            pair.previous.next = pair.next
            pair.next.previous = pair.previous
            pair.next = null
            pair.previous = null
          }
          
          // Return true
          true
        }
        else
        {
          // No pair is found with the given key, return false
          false
        }
      }
    }
  }
  
  /** Checks if the map is empty
   * 
   *  @return true if the map is empty, false otherwise */
  def isEmpty: Boolean = count == 0
  
  /** Checks if the map contains a pair with the given key
   * 
   *  @return true if the map is contains a pair with the given key, false
   *  otherwise */
  def contains(k: String): Boolean =
  {
    var isFound = false
    for(i <- 0 until bufferSize)
    {
      var pair = buffer(i)
      while(!isFound && pair != null)
      {
        isFound = pair.key == k
        pair = pair.next
      }
    }
    isFound
  }
  
  /** Gets the number of pairs in the map
   * 
   *  @return Number of pairs in the map */
  def size: Int = count
  
  /** Gives a String representation of the map */
  override def toString =
  {
    val builder = new StringBuilder("{")
    
    // Traverse the buffer and add each pair
    for(i <- 0 until bufferSize)
    {
      var pair = buffer(i)
      while(pair != null)
      {
        builder.append(pair.toString).append(",")
        pair = pair.next
      }
    }
    
    if(count > 0)
      builder.dropRight(1).append("}").toString
    else
      builder.append("}").toString
  }
}