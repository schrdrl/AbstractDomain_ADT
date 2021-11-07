package Abstraction

import Abstraction.IntegerW.*
import Abstraction.Intervals._
import java.io._
import Abstraction.Powerset.PowersetLattice
import Abstraction.{ConcreteAbstractGalois, Lattice, Powerset}

import scala.collection.immutable.Nil.{:::, head}


/**
 * Weitere Funktionen: sum, prod, ++, concat, intersect
 */

sealed trait Components //h,tail,elem

case class Head(h: Intervals) extends Components

case class Tail(a: AList[Intervals]) extends Components

case class Elem(e: AList[Intervals]) extends Components


sealed trait AList[+Intervals] { //"behaviour"
  def intervals = Intervals(mlb = IntegerNegInf, mub = IntegerInf) //initialisiert
}

case object Nil extends AList[Nothing]

case class Cons[Intervals](head: Intervals, tail: AList[Intervals]) extends AList[Intervals]

case class Many[Intervals](elem: AList[Intervals]) extends AList[Intervals]

object AList { //case class


  //Exception Handling with 'Option'
  def head[Intervals](l: AList[Intervals]): Option[Intervals] = l match {
    case Nil => None
    case Cons(h, _) => Some(h)
    case Many(e) => head(e)
  }

  def tail[Intervals](l: AList[Intervals]): Option[AList[Intervals]] = l match {
    case Nil => None
    case Cons(_, t) => Some(t)
    case Many(e) => tail(e)
  }


  //Length: 0 or IntegerInf
  def length(l: AList[Intervals]): Intervals = l match {
    case Nil => (IntegerVal(0), IntegerVal(0))
    case Cons(h, t) => IntegerVal(h) widen(IntegerVal(0), IntegerInf) //1 + length(t)
    case Many(e) => (IntegerVal(0), IntegerInf) //0 widen (1 + length(e))
    //Many: (IntegerVal(0),IntegerVal(0)) widen (IntegerVal(0),IntegerInf)

  }

  //TODO nil()
  //TODO cons()
  //TODO many()

  def sum(al: AList[Intervals]): Int = al match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
    case Many(e) => head(e) + sum(tail(e))

  }

  def product(al: AList[Intervals]): Int = al match {
    case Nil => 1
    case Cons(h, t) => h * product(t)
    case Many(e) => head(e) * product(tail(e))
  }

  //TODO concat -> Union
  def concat[Intervals](l1: AList[Intervals], l2: AList[Intervals]): AList[Intervals] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, concat(t, l2))
    case Many(e) => Cons(head(e), concat(tail(e), l2))
    //TODO Widening
  }

  //TODO intersect


  implicit val AListLattice = new Lattice[AList[Intervals]] {
    override def bot: AList[Intervals] = Nil

    override def top: AList[Intervals] = ??? //Many(e) mit Intervall (IntegerNegInf, IntegerInf)

    override def lub(a1: AList[Intervals], a2: AList[Intervals]): AList[Intervals] = (a1, a2) match {
      case (Nil, a) => a
      case (a, Nil) => a
      case (Many(e1), Cons(h1, t1)) | (Cons(h2, t2), Many(e2), ) => ??? //Many
      case (Cons(h1, t1), Cons(h2, t2)) => ??? //Cons
      case (Many(e1), Many(e2)) => ??? //Many
      //TODO Intervalle "neu berechnen"
      // a1 union a2

    }

    override def glb(a1: AList[Intervals], a2: AList[Intervals]): Option[AList[Intervals]] = (a1, a2) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Many(e), Cons(h, t)) => ???
      case (Cons(h, t), Many(e)) => ???
      case (Cons(h1, t1), Cons(h2, t2)) => ??? //Cons
      case (Many(e1), Many(e2)) => ??? //Many

      //TODO Intervalle "neu berechnen"
      // a1 intersect a2

    }


    override def <=(a1: AList[Intervals], a2: AList[Intervals]): Boolean = (a1, a2) match {
      case (Nil, _) => true
      case (Cons(h1, t1), Cons(h2, t2)) => ???
      case (Cons(h, t), Many(elem)) => ???
      case (Many(elem), Cons(h2, t2)) => ???
      case (Many(e1), Many(e2)) => ???

      //Intervalle vergleichen

    }

    override def widen(a1: AList[Intervals], a2: AList[Intervals], bound: Int): AList[Intervals] = (a1, a2) match {
      case (Nil, a) => a
      case (a, Nil) => a
      case (Cons(h1, t1), Cons(h2, t2)) => ???
      case (Cons(h, t), Many(elem)) => ???
      case (Many(elem), Cons(h2, t2)) => ???

      //Intervalle "widen" (schon implementiert) Listen "widen"
      //union (deprecated) /concat/ ::

    }
  }


  implicit val List_AList_Galois = new ConcreteAbstractGalois[List[Int], AList[Intervals]] {
    override def latticeC: Lattice[Set[List[Int]]] = PowersetLattice[List[Int]]

    override def latticeA: Lattice[AList[Intervals]] = AListLattice

    override def alpha(dcs: Set[List[Int]]): AList[Intervals] = ???

    override def gamma(da: AList[Intervals], bound: Int): Set[List[Int]] = da match {
      case Nil => ???
      case Cons(h, t) => ???
      case Many(e) => ???

    }
  }


}

object AList {
  val Nil = Nil
  val Cons = Cons(Head, Tail)
  val Many = Many(Elem)
}

/**
 * Cases:
 * -(Nil,_)
 * -Nil, Nil
 * -Nil,Cons
 * -Nil,Many
 * -(_,Nil)
 * -Nil,Nil
 * -Cons,Nil
 * -Many,Nil
 * -(Many,_)
 * -Many,Nil
 * -Many,Cons
 * -Many,Many
 * -(_,Many)
 * -Nil,Many
 * -Cons,Many
 * -Many,Many
 * -(Cons,_)
 * -Cons,Nil
 * -Cons,Cons
 * -Cons,Many
 * -(_,Cons)
 * -Nil,Cons
 * -Cons,Cons
 * -Many,Cons
 *
 *
 * => (Nil,Nil)
 * => (Nil,Cons)| (Cons,Nil)
 * => (Nil,Many)| (Many,Nil)
 * => (Many,Cons)| (Cons,Many)
 * => (Cons,Cons)
 * => (Many,Many)
 *
 */