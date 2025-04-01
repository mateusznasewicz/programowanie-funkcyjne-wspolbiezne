//Mateusz Nasewicz
import scala.annotation.tailrec

//Zadanie 1A
@tailrec
def existsA[A](xs: List[A])(p: A => Boolean): Boolean =
  xs match
    case h :: t => p(h) || existsA(t)(p)
    case Nil => false

existsA (List(5, 1, 2, 3)) (_ == 2)
existsA (List(5, 1, 2, 3)) (_ > 0)
existsA (List("Ala", "ma", "kota")) (_ == "ma")
!existsA (List(5, 1, 2, 3)) (_ == 0)
!existsA (Nil) (_ == false)

//Zadanie 1B
def existsB[A](xs: List[A])(p: A => Boolean) =
  (xs foldLeft false)((acc, x) => p(x) || acc)

existsB (List(5, 1, 2, 3)) (_ == 2)
existsB (List(5, 1, 2, 3)) (_ > 0)
existsB (List("Ala", "ma", "kota")) (_ == "ma")
!existsB (List(5, 1, 2, 3)) (_ == 0)
!existsB (Nil) (_ == false)


//Zadanie 1C
def existsC[A](xs: List[A])(p: A => Boolean) =
  (xs foldRight false)((x, acc) => p(x) || acc)

existsC (List(5, 1, 2, 3)) (_ == 2)
existsC (List(5, 1, 2, 3)) (_ > 0)
existsC (List("Ala", "ma", "kota")) (_ == "ma")
!existsC (List(5, 1, 2, 3)) (_ == 0)
!existsC (Nil) (_ == false)

//Zadanie 2
val filterR: [A] => (xs: List[A]) => (p: A => Boolean) => List[A] =
  [A] => xs => p =>
    (xs foldRight List[A]())((x,acc) => if p(x) then x :: acc else acc)

filterR (List(2,7,1,3,7,8,4,1,6,9)) (_ > 3) == List(7, 7, 8, 4, 6, 9)
filterR (List("ala", "ma", "kota")) (_ == "ala") == List("ala")
filterR (List("ala", "ma", "kota")) (_ == "pies") == List()
filterR (Nil) (_ == "pies") == List()


//Zadanie 4
def splitAt[A](xs: List[A])(n: Int): (List[A], List[A]) =
  def helper[A](xs: List[A])(n: Int)(acc: List[A]): (List[A], List[A]) =
    xs match
      case h :: t => if n > 0 then helper(t)(n-1)(h :: acc) else (acc.reverse,xs)
      case Nil => (acc.reverse,xs)
  helper(xs)(n)(Nil)

splitAt (List('a','b','c','d','e')) (2) == (List('a','b'), List('c','d','e'))
splitAt (List(1,2,3,4,5)) (2) == (List(1,2), List(3,4,5))
splitAt (List(1,2)) (3) == (List(1,2), List())
splitAt (Nil) (3) == (Nil, Nil)


