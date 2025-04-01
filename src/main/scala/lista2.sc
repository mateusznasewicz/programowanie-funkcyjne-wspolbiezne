//Mateusz Nasewicz
import scala.annotation.tailrec

//Zadanie 1
def take[A](n: Int, xs: List[A]): List[A] =
  xs match
    case head :: tail => if n > 0 then  head :: take(n-1,tail) else Nil
    case Nil => Nil

take(2, List(1,2,3,5,6)) == List(1,2)
take(-2, List(1,2,3,5,6)) == Nil
take(8, List(1,2,3,5,6)) == List(1,2,3,5,6)
take(10, Nil) == Nil
take(0,List(1,2,3)) == Nil
take(1,List(1)) == List(1)
take(2,List("Ala","ma","kota")) == List("Ala","ma")

//Zadanie 2
@tailrec
def drop[A](n: Int, xs: List[A]): List[A] =
  xs match
    case head :: tail => if n > 0 then drop(n-1,tail) else xs
    case Nil => Nil

drop(2, List(1,2,3,5,6)) == List(3,5,6)
drop(-2, List(1,2,3,5,6)) == List(1,2,3,5,6)
drop(8, List(1,2,3,5,6)) == Nil
drop(10, Nil) == Nil
drop(0,List(1,2,3)) == List(1,2,3)
drop(1,List(1)) == Nil
drop(1,List("Ala","ma","kota")) == List("ma","kota")

//Zadanie 3
def reverse[A](xs: List[A]): List[A] =
  @tailrec
  def reverseHelper(xs: List[A], reversed: List[A]): List[A] =
    xs match
      case head :: tail => reverseHelper(tail, head :: reversed)
      case Nil => reversed
  reverseHelper(xs, Nil)

reverse(List("Ala", "ma", "kota")) == List("kota", "ma", "Ala")
reverse(Nil) == Nil
reverse(List(1,2,3)) == List(3,2,1)
reverse(List("Ala")) == List("Ala")

//Zadanie 4
val replicate: List[Int] => List[Int] = xs =>
  def replicateHelper(counter: Int, element: Int, tail: List[Int]): List[Int] =
    if counter > 0 then element :: replicateHelper(counter-1,element,tail)
    else replicate(tail)
  xs match
    case head :: tail => replicateHelper(head,head,tail)
    case Nil => Nil

replicate(List(1,0,4,-2,3)) == List(1, 4, 4, 4, 4, 3, 3, 3)
replicate(Nil) == Nil
replicate(List(0,0,1)) == List(1)
replicate(List(0,0,0)) == Nil

//Zadanie 5
val root3: Double => Double = a =>
  @tailrec
  def root3Helper(x: Double): Double =
    if math.abs(x*x*x - a) <= 10e-15 * math.abs(a) then x
    else root3Helper(x + (a / (x * x) - x) /3)
  root3Helper(if a > 1 then a / 3 else a)

math.abs(root3(-8.0) + 2.0) <= 1.0e-70
math.abs(root3(8.0) - 2.0) <= 1.0e-70
math.abs(root3(1.0) - 1.0) <= 1.0e-70
math.abs(root3(0.0) + 0.0) <= 1.0e-70