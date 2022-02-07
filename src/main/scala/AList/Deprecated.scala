package AList

class Deprecated {

  /*
 //TODO deprecated
 //trait "separates" the input of a unary operations in two sets
 trait AUnOp[A]{
   def positive(a:A): Set[A]
   def negative(a:A): Set[A]
 }
//TODO deprecated -> need new version
 //checks whether an AList is empty and returns the empty and non-empty parts of it
 object AIsNil extends AUnOp[AList]{
   override def positive(a: AList): Set[AList] = a match{
     case ANil => Set(a)
     case ACons(_,_) => Set()
     case AMany(e) => Set(ANil)
   }

   override def negative(a: AList): Set[AList] = a match {
     case ANil => Set()
     case ACons(_,_) => Set(a)
     case AMany(e) => Set(ACons(e, AMany(e)))
   }
 }





  */
  /*
   //TODO deprecated
    //trait "separates" inputs of binary operations in two sets
    trait ABinOp[A]{
      def positive(a1:A,a2: A ) :  Set[AInt]
      def negative(a1:A,a2: A) : (Set[(A,A)] , Set[(A,A)])
    }
   //TODO deprecated

      object AIntEqual extends ABinOp[AInt] {
        //returns the interval both inputs have in common
        override def positive(a1: AInt, a2: AInt): Set[AInt] = {
          if (intervals.intersect_Interval(a1,a2) != intervals.Interval(IntegerInf, IntegerNegInf)) Set(intervals.intersect_Interval(a1,a2)) else Set()
        }

        //returns the interval parts that are not equal: (a1(before, after), a2(before, after))
        override def negative(a1: AInt, a2: AInt): (Set[(AInt, AInt)], Set[(AInt, AInt)]) = {
         if  (AIntEqual.positive(a1,a2).isEmpty){ //not equal at all
           (Set((a1, null)), Set((a2, null)))
         }
          else{ //there are some parts of the intervals that are equivalent
             if(a1.lb == a2.lb){  //exactly the same boundaries
               if(a1.ub == a2.ub){
                (Set((null, null)), Set((null,null)))
               }else if(IntegerW.<(a1.ub, a2.ub)){
                   (Set((null, null)), Set((null,Interval(IntegerW.+(a1.ub, IntegerVal(1)), a2.ub))))
               }else{ //if(IntegerW.<(a2.ub, a1.ub))
                  (Set((null, Interval(IntegerW.+(a2.ub, IntegerVal(1)), a1.ub))), Set((null, null)))
               }
             }else if(IntegerW.<(a1.lb, a2.lb)){
               if(a1.ub == a2.ub){
                 (Set((Interval(a1.lb,IntegerW.-(a2.lb, IntegerVal(1))), null)), Set((null, null)))
               }else if(IntegerW.<(a1.ub, a2.ub)){
                 (Set((Interval(a1.lb,IntegerW.-(a2.lb, IntegerVal(1))), null)), Set((null, Interval(IntegerW.+(a1.ub, IntegerVal(1)), a2.ub))))
               }else { //if(IntegerW.<(a2.ub, a1.ub))
                 (Set((Interval(a1.lb, IntegerW.-(a2.lb, IntegerVal(1))),Interval(IntegerW.+(a2.ub, IntegerVal(1)),a1.ub))), Set((null, null)))
               }
             }else { //if(IntegerW.<(a2.lb, a1.lb))
               if(a1.ub == a2.ub){
                 (Set((null,null)), Set((Interval(a2.lb, IntegerW.-(a1.lb, IntegerVal(1))),null)))
               }else if(IntegerW.<(a1.ub, a2.ub)){
                 (Set((null, null)), Set((Interval(a2.lb, IntegerW.-(a1.lb, IntegerVal(1))),Interval(IntegerW.+(a1.ub, IntegerVal(1)), a2.ub))))
               }else { //if(IntegerW.<(a2.ub, a1.ub))
                 (Set((null, Interval(IntegerW.+(a2.ub, IntegerVal(1)),a1.ub))), Set((Interval(a2.lb, IntegerW.-(a1.lb, IntegerVal(1))),null)))
               }
             }
         }
        }
      }


    //TODO needs improvement
    object AListEqual extends ABinOp[AList] {
      override def positive(a1: AList, a2: AList): Set[AList] = (a1,a2) match { //return the equal part -> one AList
        case (ANil, ANil) => Set(ANil)
        case (ANil, AMany(_)) | (AMany(_), ANil)=> Set(ANil)
        case (ANil, ACons(_,_)) | (ACons(_,_), ANil) => Set()
        case (ACons(h1,t1), ACons(h2,t2)) => if(AIntEqual.positive(h1,h2).nonEmpty) Set(ACons(AIntEqual.positive(h1,h2).head, AListEqual.positive(t1,t2).head)) else Set()
        case (ACons(h,t), AMany(e)) =>  if(AIntEqual.positive(h,e).nonEmpty) Set(ACons(AIntEqual.positive(h,e).head, AListEqual.positive(t,a2).head)) else Set()
        case (AMany(e), ACons(h,t)) => if(AIntEqual.positive(e,h).nonEmpty) Set(ACons(AIntEqual.positive(e,h).head, AListEqual.positive(a1,t).head)) else Set()
        case (AMany(e1), AMany(e2)) => if(AIntEqual.positive(e1,e2).nonEmpty) Set(AMany(AIntEqual.positive(e1,e2).head)) else Set()
      }


      override def negative(a1: AList, a2: AList): (Set[(AList,AList)], Set[(AList,AList)]) =(a1,a2) match {
        case (ANil, ANil) => (Set(), Set())
        case (ANil, AMany(_)) => ???
        case(AMany(_), ANil) => ???
        case (ANil, ACons(_,_)) => ???
        case (ACons(_,_), ANil) => ???
        case (ACons(h1,t1), ACons(h2,t2)) => ???
        case (ACons(h,t), AMany(e)) =>  ???
        case (AMany(e), ACons(h,t)) => ???
        case (AMany(e1), AMany(e2)) => ???
      }

  */

}
