package com.linovi.assertionsandtests

import org.scalatest._

class MyPoint(val x: Int, val y: Int)
{
  if(x > 20) throw new IllegalArgumentException
  
  def inBounds = x >= 0 && y >= 0
}

class MySpec extends FlatSpec with ShouldMatchers
{
  "A point(x, y)" should "be in bounds if x and y are > 0 " in
  {
    val p = new MyPoint(3, 5)
    p.inBounds should be (true)
  }
  
  it should "throw an IAE if passed x is > 20" in
  {
    evaluating
    {
      new MyPoint(23, 4)
    } should produce [IllegalArgumentException]
  }
}