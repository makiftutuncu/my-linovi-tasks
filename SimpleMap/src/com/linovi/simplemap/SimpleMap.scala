package com.linovi.simplemap

/**
 * A simple implementation of a String to String map
 */
class SimpleMap
{
  /** Buffer in which the map items will be stored */
  private val buffer = new LinkedList
  
  // TODO Implement
  def get(k: String): String = ???
  def put(k: String, v: String): Boolean = ???
  def delete(k: String): Boolean = ???
  def isEmpty: Boolean = ???
  def contains(k: String): Boolean = ???
  def size: Int = ???
  override def toString = ???
}