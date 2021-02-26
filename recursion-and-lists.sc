def squeeze[T,A](li:List[T], func: (T, T) => A): List[A] = {
  @scala.annotation.tailrec
  def squeezeT(li: List[T], func: (T, T) => A, accumulator: List[A]): List[A] = {
    if (li.length <= 1) accumulator.reverse
    else squeezeT(li.tail, func, func(li.head, li.tail.head) :: accumulator )
  }
  squeezeT(li, func, List())
}

squeeze(List(1), (a: Int, b:Int) => 0.5 * (a + b))
squeeze(List(1), (a: Int, b:Int) => b - a)

squeeze(List(1, 2, 3, 4, 5), (a: Int, b:Int) => 0.5 * (a + b))
squeeze(List(1, 2, 3, 4, 10), (a: Int, b:Int) => b - a)

squeeze(List(1, 2, 3, 4, 5, 6), (a: Int, b:Int) => 0.5 * (a + b))
squeeze(List(1, 2, 3, 4, 5, 10), (a: Int, b:Int) => b - a)

squeeze(List("a", "ba", "ciop", "dkolo"), (a: String, b: String) => a.length + b.length)

def flatten[T](li:List[List[T]]):List[T] = {
  @scala.annotation.tailrec
  def flattenTail(li:List[List[T]], accumulator:List[T]):List[T] = li match {
    case Nil => accumulator
    case _ => flattenTail(li.tail, accumulator ::: li.head)
  }
  flattenTail(li, List())
}

flatten(Nil)
flatten(List(List(1, 2), List(3, 4)))
flatten(List(List(List(1, 2)), List(List(3, 4))))
