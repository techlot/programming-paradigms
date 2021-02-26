def remove_repetitions_rec[T](elems: List[T]): List[T] = elems match {
  case Nil => elems
  case hd :: tl => hd :: remove_repetitions_rec(tl filter (_ != hd))
}

remove_repetitions_rec(List())
remove_repetitions_rec(List(9, 9, 9, 9))
remove_repetitions_rec(List(0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1))
remove_repetitions_rec(List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
remove_repetitions_rec(List(0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0))
remove_repetitions_rec(List(1, 1, 4, 3, 3, 1, 1, 6, 1))
remove_repetitions_rec(List('Q', 'Q', 'W', 'E', 'Q', 'E', 'R', 'R', 'T', 'Y', 'Q'))
remove_repetitions_rec(List(1.0, 2.0, 3.0, 1.0, 3.0, 1.0, 2.0, 4.0))

def remove_repetitions_imp[Q: Manifest](elems: Array[Q]): Array[Q] = {
  val def_size = elems.length
  var new_size = 0

  for (i <- 0 until def_size) {
    var repeated = false

    for (j <- i + 1 until def_size) {
      if (elems(i) == elems(j)) {
        repeated = true
      }
    }

    if (!repeated) {
      new_size += 1
    }
  }
  
  val result = new Array[Q](new_size)
  if (new_size != 0) {
    result(0) = elems(0)
  }

  for (i <- 1 until new_size) {
    var flag = false

    for (j <- 0 until def_size; if !flag) {
      if (!is_added(result, elems(j))) {
        result(i) = elems(j)
        flag = true
      }
    }
  }

  def is_added(arr: Array[Q], target: Q): Boolean = {
    val len = arr.length
    var flag = false
    for (i <- 0 until len) {
      if (arr(i) == target) {
        flag = true
      }
    }
    flag
  }
  result
}

remove_repetitions_imp(Array[Int]())
remove_repetitions_imp(Array(9, 9, 9, 9, 9))
remove_repetitions_imp(Array(0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1))
remove_repetitions_imp(Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0))
remove_repetitions_imp(Array(0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0))
remove_repetitions_imp(Array(1, 1, 4, 3, 3, 1, 1, 6, 1))
remove_repetitions_imp(Array('Q', 'Q', 'W', 'E', 'Q', 'E', 'R', 'R', 'T', 'Y', 'Q'))
remove_repetitions_imp(Array(1.0, 2.0, 3.0, 1.0, 3.0, 1.0, 2.0, 4.0))
