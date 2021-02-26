def unzipL[Q, Z](list: LazyList[(Q, Z)]): (LazyList[Q], LazyList[Z]) =
  list match {
      case LazyList() => (LazyList(), LazyList())
      case hd #:: tl => {
        lazy val result = unzipL(tl)
        (hd._1 #:: result._1, hd._2 #:: result._2)
      }
}

val ll_0 = LazyList()
val ll_1 = LazyList( (1, 2) )
val ll_2 = LazyList( ('A', 'D'), ('B', 'C'), ('C', 'B'), ('D', 'A') )
val ll_3 = LazyList( ("One", 2), (1, "Two"), ("Three", 4), (3, "Four")  )
val ll_4 = LazyList( ("Left", "Right"), ("Left", "Right"), ("Left", "Right"), ("Left", "Right"), ("Left", "Right"), ("Left", "Right") )

val vals = LazyList.from(1).zip(LazyList.from(1).map((n: Int) => n * n))


unzipL(ll_0)
unzipL(ll_1)
unzipL(ll_2)
unzipL(ll_3)
unzipL(ll_4)

val vals_res = unzipL(vals)

vals_res._1.take(10).force
vals_res._2.take(10).force

sealed trait LList[+T]
case object LNil extends LList[Nothing]
case class LCons[T](head: T, tail: () => LList[T]) extends LList[T]

def filterL[Q](li: LList[Q])(func: Q => Boolean): LList[Q] =
  li match {
    case LNil => LNil
    case LCons(hd, tl) => if (func(hd)) LCons(hd, () => filterL(tl())(func))
                          else filterL(tl())(func)
  }

def takeL[Q](li: LList[Q], n: Int): List[Q] =
  (li, n) match {
    case (LNil, _) => Nil
    case (_, k) if k <= 0 => Nil
    case (LCons(hd, tl), _) => hd :: takeL(tl(), n - 1)
  }

val integers: LList[Int] = {
  def next(n: Int): LList[Int] = {
    LCons(n, () => next(n + 1))
  }
  next(0)
}

val strings: LList[String] = {
  def next(n: String): LList[String] = {
    LCons(n, () => next(n.concat("A")))
  }
  next("A")
}

val remove_odds = filterL(integers)((x: Int) => x % 2 != 0)
val remove_evens = filterL(integers)((x: Int) => x % 2 == 0)
val remove_less = filterL(integers)((x: Int) => x < 1055)
val remove_divisible_by_3 = filterL(integers)((x: Int) => x % 3 == 0)

val rem_str_less = filterL(strings)((s:String) => s.size < 5)
val rem_str_even_size = filterL(strings)((s:String) => s.size % 2 == 0)
val rem_str_odd_size = filterL(strings)((s:String) => s.size % 2 != 0)

takeL(integers, 100)

takeL(remove_odds, 100)
takeL(remove_evens, 100)
takeL(remove_divisible_by_3, 100)
takeL(remove_less, 100)

takeL(strings, 15)

takeL(rem_str_even_size, 10)
takeL(rem_str_odd_size, 10)
takeL(rem_str_less, 5)
