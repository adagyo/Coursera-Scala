package funsets

object Main extends App {
  import FunSets._
  //println(contains(singletonSet(1), 1))


  val s0 = singletonSet(0)
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = singletonSet(4)
  val s6 = singletonSet(6)

  val s12 = union(s1,s2)
  val s13 = union(s1,s3)
  val s23 = union(s2,s3)
  val s246 = union(union(s2,s4),s6)
  val s32 = union(s3,s2)

  val s1000 = singletonSet(1000)
  val s1000m = singletonSet(-1000)
  val s1000m0 = union(s0, s1000m)

  val s246_1000 = union(s246, s1000)


  //printSet(union())

  println("unions")
  val s123 = union(s12,s3)
  printSet(s123)
  val s1234 = union(s123,s4)
  printSet(s1234)

  println("Filters")
  val filtered = filter(s123, x => x % 2 == 0)
  printSet(filtered)
  val filtered2 = filter(s1234, x => x % 2 == 0)
  printSet(filtered2)

  println("ForAlls")
  println("Les éléments de s123 sont ils tous impairs: " + forall(s123, x => x % 2 == 1))
  println("Les éléments de s246 sont ils tous pairs: " + forall(s246, x => x % 2 == 0))
  println("Les éléments de s123 sont ils tous < 10: " + forall(s123, x => x < 10))

  println("Exists")
  println("Y a t'il un element impair dans s123:" + exists(s123, x => x % 2 == 1))
  println("Y a t'il un element impair dans s246:" + exists(s246, x => x % 2 == 1))

  println("Maps")
  printSet(map(s123, x => x * 2))
  printSet(map(s1234, x => x % 2))
  printSet(map(s1234, x => x * x))
  printSet(map(s246_1000, x => x - 1))

}
