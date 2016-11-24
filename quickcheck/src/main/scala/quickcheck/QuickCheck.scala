package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("insertTWO") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(deleteMin(h)) == Math.max(a, b)
  }

  property("deleteMin1") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  def toList(h: H):List[A] = if (isEmpty(h)) Nil else findMin(h)::toList(deleteMin(h))

  property("sorted1") = forAll { (h: H) =>
    val list = toList(h)
    list == list.sorted
  }

  property("sorted2") = forAll { (a: Int, b: Int, c: Int) =>
    val list = List(a, b, c).sorted
    val h = (list foldLeft empty)((h, el) => insert(el, h))
    toList(deleteMin(h)) == list.tail
  }

}
