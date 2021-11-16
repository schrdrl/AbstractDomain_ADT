package Abstraction




/**
 * TODO add Documentation
 *
 * @param intervals
 */
case class ALists(intervals: Intervals){
  import intervals.Interval //import inner Class
  type AInt = Interval  //alias


  sealed trait AList  //"behaviour"
  case object ANil extends AList
  case class ACons(head: AInt, tail: AList) extends AList
  case class AMany(elem: AInt) extends AList

  sealed trait AOption[+A]
  case object ANone extends AOption[Nothing]
  case class ASome[A](get: A) extends AOption[A]
  case class AMaybe[A](get: A) extends AOption[A]

  def aHead (l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(h, _) => ASome(h)
    case AMany(e) => AMaybe(e) //AMany = ANil ≀ ACons(e, Many(e))
  }



  def aTail (l: AList): AOption[AInt] = l match {
    case ANil | ACons(_, ANil) => ANone


    case ACons(_, ACons(h,t)) => ???

    case ACons(_, AMany(e)) =>AMaybe(e)
    case AMany(e) => AMaybe(e) //AMany = ANil ≀ ACons(e, Many(e))
  }


  def aLength(l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(_, _) => ASome(Interval(IntegerVal(1), IntegerInf))
    case AMany(_) => ASome(Interval(IntegerVal(0), IntegerInf))
  }



  def isConcreteElementOf_Int(i: Int, ai: AInt): Boolean ={
    intervals.contains(ai, i)
  }


  def isConcreteElementOf_List(l: List[Int], al:AList): Boolean = (l, al) match{
    case (Nil, ANil) => true
    case (Nil, _) => false
    case (Nil, AMany(_)) => true ////AMany = ANil ≀ ACons(e, Many(e))
    case (x::xs, ANil) => false
    case (x::xs, ACons(ax, axs)) =>
      isConcreteElementOf_Int(x, ax) && isConcreteElementOf_List(xs, axs)
      //\gamma(ACons(ax, axs)) = ???
      // x = 1, xs = 2::3::Nil
      // ax [-1,+7], axs = ???

      ???
    case (x::xs, AMany(ax)) =>
      isConcreteElementOf_Int(x, ax) && isConcreteElementOf_List(xs, al)
      // \gamma(AMany(ax)) = \gamma(ANil) \union \gamma(ACons(ax, AMany(ax)))
      ???

  }



  def isConcreteElementOf_Option[Int](o: Option[Int], ao: AOption[AInt]): Boolean = (o,ao) match {
    case (None, ANone) => true
    case (None, _) => false
    case (None, AMaybe(_)) => true
    case (Some(_), ANone) => false
    case (Some(_), ASome(_)) => ???
    case (Some(_), AMaybe(_)) => ???
    // \gamma(AMaybe(ax)) = \gamma(ANone) \union \gamma(ASome(_)))
  }



  def widening (l1: AList, l2: AList): AList = (l1,l2) match {
    case (ANil , ANil) => ANil
    case (ANil, ACons(_,_)) | (ACons(_,_), ANil)=> ???
    case (ANil, AMany(_)) | (AMany(_), ANil)=> ???
    case (ACons(_,_), ACons(_,_)) => ???
    case (ACons(_,_), AMany(_)) | (AMany(_), ACons(_,_)) => ???
    case (AMany(_), AMany(_)) => ???
  }

  def gamma(da: AList, bound: Int): List[Int] = da match {
    case ANil => Nil
    case ACons(h, t) => ???
    case AMany(e) => ???
  }


/*



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








