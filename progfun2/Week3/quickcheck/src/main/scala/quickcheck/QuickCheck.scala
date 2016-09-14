package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    elem <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(elem, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a1: A, a2: A) =>
    val h = insert(a1, insert(a2, empty))
    if(a1 < a2) {
      findMin(h) == a1
    } else {
      findMin(h) == a2
    }
  }

  property("del1") = forAll { a: A =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("recDelete") = forAll { h: H =>
    def rec(h: H, acc: List[A]): List[A] = {
      if(isEmpty(h)) acc
      else rec(deleteMin(h), findMin(h) :: acc)
    }

    def isSorted(l: List[A]): Boolean = l match {
      case Nil => true
      case _head :: _tail =>
        if(_tail.isEmpty) true
        else if(_head < _tail.head) false
        else isSorted(_tail)
    }

    isSorted(rec(h, List.empty))
  }

  property("miniMeld") = forAll { (h1: H, h2: H) =>
    val h12 = meld(h1,h2)
    val min_h1 = if(isEmpty(h1)) 0 else findMin(h1)
    val min_h2 = if(isEmpty(h2)) 0 else findMin(h2)
    val min_h12 = if(isEmpty(h12)) 0 else findMin(h12)

    if(min_h1 < min_h2) min_h1 == min_h12
    else min_h2 == min_h12
  }

  /*property("insert1") = forAll { (a1: A, a2: A) =>
    val h = insert(a1, insert(a2, empty))

  }*/
  property("del2") = forAll { (a1: A, a2: A) =>
    val h_1 = insert(a1, insert(a2, empty))
    val min_h_1 = findMin(h_1)
    val h_2 = deleteMin(h_1)
    val min_h_2 = findMin(h_2)
    if(a1 != a2) min_h_2 != min_h_1
    else min_h_2 == min_h_1
  }

}
