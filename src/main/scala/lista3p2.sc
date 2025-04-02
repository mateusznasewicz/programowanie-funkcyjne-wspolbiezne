//Mateusz Nasewicz
import scala.annotation.tailrec

//Zadanie 4
def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) =
  @tailrec
  def helper[A](tail: List[A])(n: Int)(acc: List[A]): (List[A], List[A]) =
    tail match
      case h :: t => if n > 0 then helper(t)(n-1)(h :: acc) else (acc.reverse,tail)
      case Nil => (acc.reverse,tail)
  helper(xs)(n)(Nil)

splitAt (List('a','b','c','d','e')) (2) == (List('a','b'), List('c','d','e'))
splitAt (List(1,2,3,4,5)) (2) == (List(1,2), List(3,4,5))
splitAt (List(1,2)) (3) == (List(1,2), List())
splitAt (Nil) (3) == (Nil, Nil)