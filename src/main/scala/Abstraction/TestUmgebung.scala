package Abstraction

import Abstraction.AList._

object TestUmgebung {

  def main(args: Array[String]): Unit = {
    val emptyList :AList[Nothing] = Nil
    val notEmptyList : AList[Intervals] = Cons(IntegerVal(1), emptyList)
    println(head(notEmptyList))
    println(tail(notEmptyList))

  }
}

