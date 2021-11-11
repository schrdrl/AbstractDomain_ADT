package Abstraction

import Abstraction.IntegerW.{*, max, min}
import Abstraction.Intervals._

import java.io._
import Abstraction.Powerset.PowersetLattice
import Abstraction.{ConcreteAbstractGalois, Lattice, Powerset}

import scala.collection.immutable.Nil.{:::, head}
import scala.runtime.Nothing$

/**
 * TODO add Documentation
 *
 * @param intervals
 */
case class ALists(intervals: Intervals) {
  import intervals.Interval //import inner Class
  type AInt = Interval  //alias


  sealed trait AList //"behaviour"
  case object ANil extends AList
  case class ACons(head: AInt, tail: AList) extends AList
  case class AMany(elem: AInt) extends AList

  sealed trait AOption[+A]
  case object ANone extends AOption[Nothing]
  case class ASome[A](get: A) extends AOption[A]
  case class AMaybe[A](get: A) extends AOption[A]

  def head (l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(h, _) => ASome(h)
    case AMany(e) => AMaybe(e) //AMany = ANil ≀ ACons
  }

  /**
   * TODO: case ACons
   *  -> without asking for variable 'Interval'
   *  -> not recursive
   *  t = AMaybe ANone ≀ (Interval(IntegerNegInf, IntegerInf))
   *    -> split in two cases
   */
  def tail (l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(_, ANil) => ANone
    case ACons(_, t) => AMaybe(Interval(IntegerNegInf, IntegerInf)) // TODO check intervall
    case AMany(e) => AMaybe(e)
  }

  def length(l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(_, _) => ASome(Interval(IntegerVal(1), IntegerInf))
    case AMany(_) => ASome(Interval(IntegerVal(0), IntegerInf)) //TODO check ASome or AMaybe
  }

  //TODO Option[Int], AOption[AInt]
  def isConcreteElementOf_Int(i: Int, ai: AInt): Boolean ={
    intervals.contains(ai, i)
  }


  def isConcreteElementOf_List(l: List[Int], al:AList): Boolean = (l, al) match{
    case (Nil, ANil) => true
    //case (Cons(h,t), ACons(e))
    //case (Cons(h,t), AMany(e))
  }

  def isConcreteElementOf_Option(o: Option[Int], ao: AOption[AInt]): Boolean = (o,ao) match {
    case (None, ANone) => true
    //case (Some, ASome)
    //case (Some, AMaybe)
  }
/**




  def minList(l: AList[IntegerW]) : IntegerW = l match {
    case Nil => ???
    case Cons(h,t) => ???
    case Many(e) => ???
  }

  def maxList(l: AList[IntegerW]) : IntegerW = l match {
    case Nil => ???
    case Cons(h,t) => ???
    case Many(e) => ???
  }

  //TODO nil() -> 'equivalent' to IntegerW
  //TODO cons() -> ::
  //TODO many() -> ::


  def concat(l1: AList[Intervals], l2: AList[Intervals]): AList[Intervals] = l1 match {
    case Nil => l2
    case Cons(h, t) => ???
    case Many(e) => ???
  }

  def intersect[Intervals](l1: AList[Intervals], l2: AList[Intervals]): AList[Intervals] = l1 match {
    case Nil => l2
    case Cons(h, t) => ???
    case Many(e) => ???
  }

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

*/
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


/**
 * Main Methods:
 * -head
 * -tail
 * -length
 * -interval
 * -minList
 * -maxList
 * -cons
 * -nil
 *
 * instances:
 * -Lattice
 * -ConcreteAbstractGalois
 *
 * Extensions:
 * -sum
 * -prod
 * -concat
 * -intersect
 */

