import scala.annotation.tailrec
//Mateusz Nasewicz

//Zadanie 3a
val remove1A: [A] => (xs: List[A]) => (p: A => Boolean) => List[A] =
  [A] => xs =>  p =>
    xs match
      case Nil => Nil
      case h :: t => if p(h) then t else h :: remove1A(t)(p)

remove1A(List(1,2,3,2,5))(_ == 2) == List(1, 3, 2, 5)
remove1A(List(1,2,3,4,5))(_ % 2 == 1) == List(2, 3, 4, 5)
remove1A(List(1,2,-3,4,5))(_ < 0) == List(1, 2, 4, 5)
remove1A(List("a", "b", "c"))(_ == "b") == List("a", "c")
remove1A(Nil)(_ == 0) == Nil

//Zadanie 3b
def remove1B[A](xs: List[A])(p: A => Boolean): List[A] =
  @tailrec
  def helper(xs: List[A], acc: List[A]): List[A] =
    xs match
      case Nil => acc.reverse
      case h :: t => if p(h) then t.reverse_:::(acc) else helper(t, h :: acc)

  helper(xs, Nil)

remove1B(List(1,2,3,2,5))(_ == 2) == List(1, 3, 2, 5)
remove1B(List(1,2,3,4,5))(_ % 2 == 1) == List(2, 3, 4, 5)
remove1B(List(1,2,-3,4,5))(_ < 0) == List(1, 2, 4, 5)
remove1B(List("a", "b", "c"))(_ == "b") == List("a", "c")
remove1B(Nil)(_ == 0) == Nil
