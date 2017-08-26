package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    heap <- oneOf[H](empty, genHeap)
  } yield insert(n, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("order") = forAll { (a:Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (if (a > b) b else a)
  }

  property("empty") = forAll { a:Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("melding") = forAll { (a:Int, b: Int) =>
    val h = insert(a, empty)
    val i = insert(b, empty)
    val merged = meld(h, i)

    findMin(merged) == (if (a > b) b else a)
  }

  property("melding heaps") = forAll { (h:H, i:H) =>
    val minH = findMin(h)
    val minI = findMin(i)
    val merged = meld(h, i)
    findMin(merged) == (if (minH < minI) minH else minI)
  }

  property("associative meld") = forAll { (h:H, i:H, j:H) =>
    val a = meld(meld(h, i), j)
    val b = meld(h, meld(i, j))
    toList(a) == toList(b)
  }

  property("order of mins") = forAll { (h:H) =>
    toList(h).zip(toList(h).drop(1)).forall {
      case (x, y) => x <= y
    }
  }

  def toList(h:H):List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))



}
