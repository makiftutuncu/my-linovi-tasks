package com.linovi.treeset

/** A simple mutable stack node */
class MyStackNode(n: ImmutableNode)
{
  val node = n
  var next: MyStackNode = _
}

/** A simple mutable stack implementation holding ImmutableNode for ImmutableTreeSet */
class MyStack
{
  private var top: MyStackNode = _
  private var count: Int = 0
  
  def push(node: ImmutableNode): MyStack =
  {
    val newTop = new MyStackNode(node)
    newTop.next = top
    top = newTop
    count += 1
    
    this
  }
  
  def pop(): Option[ImmutableNode] =
  {
    if(count == 0)
      None
    else
    {
      val myTop = top
      top = top.next
      count -= 1
      
      Option(myTop.node)
    }
  }
  
  def size = count
}