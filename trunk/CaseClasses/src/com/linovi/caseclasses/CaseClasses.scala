package com.linovi.caseclasses

sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

object CaseClasses
{
  def simplifyTop(expr: Expr): Expr = expr match
  {
    case UnOp("-", UnOp("-", e)) => e  // Double negation
    case BinOp("+", e, Number(0)) => e // Adding zero
    case BinOp("*", e, Number(1)) => e // Multiplying by one
    case _ => expr
  }

  def main(args: Array[String])
  {
    // Automatic factory methods, no new keyword is needed
    val v = Var("x")
    val op = BinOp("+", Number(1), v)

    // Parameters are set as val fields automatically
    println(v.name)

    // Natural toString, hashCode and equals methods are generated automatically
    println(op)
    op.right == Var("x")

    // Copy method is generated automatically, unspecified fields are copied directly
    val op2 = op.copy(operator = "-")
  }
}