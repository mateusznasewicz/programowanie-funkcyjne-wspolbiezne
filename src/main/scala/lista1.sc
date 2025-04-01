//Mateusz Nasewicz

//Zadanie 1
val suma: List[Double] => Double = xs =>
  if xs == Nil then
    0.0
  else
    xs.head + suma(xs.tail)

suma(Nil) == 0.0
suma(List(-1, 2, 3)) == 4.0
suma(List(5.6)) == 5.6

//Zadanie 2
def ends[A](xs: List[A]): (A,A) =
    if xs == Nil then
        throw new NoSuchElementException("Empty list")
    else if xs.tail == Nil then
        (xs.head, xs.head)
    else if xs.tail.tail == Nil then
        (xs.head, xs.tail.head)
    else
        ends(xs.head :: xs.tail.tail)

ends(List(1, 3, 5, 6, 9)) == (1,9)
ends(List("Ala", "ma", "kota")) == ("Ala", "kota")
ends(List(1)) == (1,1)
ends(Nil)

//Zadanie 3
val posortowana: List[Int] => Boolean = xs =>
  if xs == Nil || xs.tail == Nil then
    true
  else
    xs.head <= xs.tail.head && posortowana(xs.tail)

posortowana(List(1,3,3,5,6,7))
!posortowana(List(11,3,3,-5,6,7))
posortowana(List())
posortowana(List(1))

//Zadanie 4
val glue: (List[String], String) => String = (xs,sep) =>
  if xs == Nil then
      ""
  else if xs.tail == Nil then
      xs.head
  else
      s"${xs.head}$sep${glue(xs.tail, sep)}"

glue(List("To", "jest", "napis"), "-") == "To-jest-napis"
glue(Nil,"-") == ""
glue(List("To","jest","napis"),"") == "Tojestnapis"
glue(List("","",""),"") == ""
glue(List("","",""),"-") == "--"
glue(List("Ala","ma","kota"),"++") == "Ala++ma++kota"