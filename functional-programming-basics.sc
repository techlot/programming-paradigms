def reverseTuple[T](pair:(T, T)): (T,T) = (pair._2, pair._1)

reverseTuple(("Hello", 10))
reverseTuple((1, 2))

def returnSecondElem[T](list: List[T]): Option[T] = {
  if (list.isEmpty) None
  else if (list.size == 1) None
  else Some(list.tail.head)
}

val l = List(1, 2, 3, 4, 5)
val l0 = List('B', 'C')
val l1 = List()
val l2 = List(1)

returnSecondElem(l)
returnSecondElem(l0)
returnSecondElem(l1)
returnSecondElem(l2)

def length[T](list: List[T]): Int = {
  if (list.isEmpty) 0
  else length(list.tail) + 1
}

def lengthTail[T](list: List[T]): Int = {
  @scala.annotation.tailrec
  def lengthT[A](list: List[A], accumulator: Int): Int = {
    if (list.isEmpty) accumulator
    else lengthT(list.tail, accumulator + 1)
  }
  lengthT(list, 0)
}


length(l)
length(l0)
length(l1)
length(l2)

lengthTail(l)
lengthTail(l0)
lengthTail(l1)
lengthTail(l2)

def reverseReg[T](list: List[T]): List[T] = {
  if (list.isEmpty) List()
  else reverseReg(list.tail) :+ list.head
}

reverseReg(l)
reverseReg(l0)
reverseReg(l1)
reverseReg(l2)

def reverseTail[T](list: List[T]): List[T] = {
  @scala.annotation.tailrec
  def reverseT[A](list: List[A], accumulator: List[A]): List[A] = {
    if (list.isEmpty) accumulator
    else reverseT(list.tail,  list.head +: accumulator )
  }
  reverseT(list, List())
}

reverseTail(l)
reverseTail(l0)
reverseTail(l1)
reverseTail(l2)

def powerSet[A](s: List[A]) = {
  @scala.annotation.tailrec
  def powerSet_rec(acc: List[List[A]], remaining: List[A]): List[List[A]] = remaining match {
    case Nil          => acc
    case head :: tail => powerSet_rec(acc ::: acc.map(_ :+ head), tail)
  }
  powerSet_rec(List(List.empty[A]), s)
}

powerSet(List(1,2,3))

def returnSecondElemPM[T](list: List[T]): Option[T] = list match {
  case Nil => None
  case _ => Some(list.tail.head)
}

val li1 = List(1, 2, 3, 4, 5)
val li2 = List('O', 'X', 'O', 'O')
val li3 = List("First", "Second")

returnSecondElem(li1)
returnSecondElem(li2)
returnSecondElem(li3)

def replaceNth[T](list: List[T], n: Int, value: T): List[T] = (list, n) match {
    case (Nil, _) => Nil
    case (_, n) if (n < 0 || n > list.length) => list
    case (_, t) if (t == 0) => value +: list.tail
    case (_, _) => list.head +: replaceNth(list.tail, n - 1, value)
}

replaceNth(li1,2,100)

def map[T](f: T => _, list: List[T]): List[_] = list match {
  case Nil => Nil
  case hd::tl => f(hd) +: map(f, tl)
}

map((x: Int) => x * x, List(0, 1, 2, 3, 4)) // Result: List(0, 1, 4, 9, 16)
map((s: String) => s.length, List("I", "Love", "Scala"))
