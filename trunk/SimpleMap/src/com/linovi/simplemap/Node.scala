package com.linovi.simplemap

/**
 * A simple data structure corresponding to a node in {@link LinkedList}
 */
class Node
{
  /** Data of the node which is an array of String because both key and value
   *  are of type String
   *  - First element will be the key
   *  - Second element will be the value
   */
  private var data: Array[String] = _
  /** Previous node that this node will point */
  var previous: Node = _
  /** Next node that this node will point */
  var next: Node = _
  
  /** Constructor setting the data at the beginning */
  def this(k: String, v: String) = {this(); setData(k, v)}
  
  /** Constructor setting the data at the beginning */
  def this(d: Array[String]) = {this(); setData(d)}
  
  /** Gets the key of the data of this node */
  def key: String = if(data != null) data(0) else ""
  
  /** Gets the value of the data of this node */
  def value: String = if(data != null) data(1) else ""
  
  /** Sets new data for this node */
  def setData(k: String, v: String): Unit =
  {
    /** Check the given values */
    if(k != null)
      setData(Array(k, v))
    else
      throw new IllegalArgumentException("Invalid data: Key should not be null!")
  }
  
  /** Sets new data for this node */
  def setData(d: Array[String]): Unit =
  {
    /** Check the given value */
    if(d != null && d.length == 2 && d(0) != null)
      data = d
    else
      throw new IllegalArgumentException("Invalid data: An array of String with two elements as a non-null key and value is required!")
  }
  
  /** Sets a new value to the data of this node */
  def setValue(v: String): Unit = if(data != null) data(1) = v
  
  /** Gives a String representation of the data of this node */
  override def toString = key + " => " + value
}