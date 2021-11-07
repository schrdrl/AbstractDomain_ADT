package Abstraction

import Abstraction.AList.{head, tail}

object TestUmgebung {

  def main(args: Array[String]): Unit = {
    val emptyList :AList[Nothing] = Nil
    val notEmptyList : AList[Intervals] = Cons(Intervals.Positive, emptyList)
    println(head(notEmptyList))
    println(tail(notEmptyList))

  }
}

