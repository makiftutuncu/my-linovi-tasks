package com.linovi.treeset

/** Represents a node in the ImmutableTreeSet */
case class ImmutableNode(value: Int, left: ImmutableNode, right: ImmutableNode)
{
  override def toString = value.toString + " (" + hashCode() % 1000 + ")"
}

object ImmutableNode
{
  def apply(v: Int) = new ImmutableNode(v, null, null)
}