package com.linovi.assertionsandtests

import org.scalatest._

class MyTestSuite extends Suite
{
  def testMyMessage() =
  {
    val message = "Hello"
    assert(message === "World")
  }
}

class MyFunSuite extends FunSuite
{
  test("Message should be equal to \"Hello\"")
  {
    val message = "Hello"
    assert(message === "World")
  }
  
  test("Expecting 1")
  {
    expect(1)
    {
      2
    }
  }
  
  test("Watch out for the illegal argument!")
  {
    intercept[IllegalArgumentException]
    {
      throw new NullPointerException
    }
  }
}