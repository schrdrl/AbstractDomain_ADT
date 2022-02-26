package tests_clean_code
import AList_CleanCode.{ACons, AInt, AMany, ANil}
import org.scalatest.funsuite.AnyFunSuite
class BasicFuntionTests extends AnyFunSuite {

  test("union:AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))


    println(d.union(e))
    println(d.union(f))
    println(e.union(e))
    println(e.union(f))
  }

  test("intersect:AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))

    println(d.intersect(e))
    println(d.intersect(f))
    println(e.intersect(e))
    println(e.intersect(f))
  }

  test("subset:AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))

    println(d.subset(e))
    println(d.subset(f))
    println(f.subset(d))
    println(e.subset(e))
    println(e.subset(f))
  }

}
