package com.linovi.simplemap

/**
 * A simple data structure corresponding to a pair in {@link SimpleMap}
 */
class Pair(val key: String, var value: String)
{
  /** Previous node that this node will point */
  var previous: Pair = _
  
  /** Next node that this node will point */
  var next: Pair = _
  
  /** Gives a String representation of the data of this node */
  override def toString = key + " => " + value
}