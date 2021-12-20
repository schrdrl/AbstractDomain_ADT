package AList_Set

import AList.Intervals


case class ALists(intervals: Intervals) {

  import intervals.Interval
  type AInt = Interval

  sealed trait AList
  case object ANil extends AList
  case class ACons(head: AInt, tail: AList) extends AList
  case class AMany(elem: AInt) extends AList


  sealed trait AOption[+A]
  case object ANone extends AOption[Nothing]
  case class ASome[A](get: A) extends AOption[A]
  case class AMaybe[A](get: A) extends AOption[A]

  sealed trait ABool
  case object ATrue extends ABool
  case object AFalse extends ABool
  case object AUnknown extends ABool


/**
 * Meeting 30.11
 * AList as Set -> without AMany, AMaybe and AUnknown
 *
 * Meeting 14.12
 * AList with AMany and AMaybe
 */

  /*Method returns the head of aList object, which is of type AOption[AInt]
*/
  def aHead (l: Set[AList]): Set[Set[AOption[AInt]]] = l.map{
    case ANil => Set(ANone)
    case ACons(h, _) => Set(ASome(h))
    case AMany(e) => Set(ANone, ASome(e))//AMaybe(e) //TODO AMaybe = = ANil ≀ ACons(e, Many(e))
  }


  def aTail (l: Set[AList]): Set[Set[AOption[AList]]]= l.map{
    case ANil => Set(ANone)
    case ACons(_,t) => Set(ASome(t))
    case AMany(e) => Set(ANone, ASome(AMany(e)))
      //AMaybe(AMany(e))
  }

  def isNil(l: Set[AList]) : Set[Set[ABool]]= l.map{
    case ANil => Set(ATrue)
    case ACons(_,_)  => Set(AFalse)
    case AMany(_) => Set(ATrue,AFalse)
  }



}

