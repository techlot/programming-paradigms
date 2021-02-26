def repeat_functional[Q](elems: List[Q], n: List[Int]): List[Q] = (elems, n) match {
  case (Nil, _) => List()
  case (_, Nil) => List()
  case (hd :: tl, n :: ns) => if (n > 0) hd :: repeat_functional(hd::tl, n - 1 :: ns)
                              else repeat_functional(tl, ns)
}

repeat_functional(List(2, 3, 4, 5), List(2, 0, -3, 3))
repeat_functional(List(2, 3, 4, 5), List())
repeat_functional(List(), List(2, 0, -3, 3))
repeat_functional(List(2, 3, 4, 5), List(2, -3, 3))
repeat_functional(List('a', 'b', 'c'), List(3, 0, 8))

def repeat_imperative[Q : Manifest](elems: Array[Q], n: Array[Int]): Array[Q] = {
  val elems_len = elems.length
  val n_len = n.length
  val min_size = math.min(elems_len, n_len)

  def calculate_new_size(): Int = {
    var new_size = 0
    for (i <- 0 until min_size) {
      if (n(i) > 0) {
        new_size += n(i)
      }
    }
    new_size
  }

  val result = new Array[Q](calculate_new_size())

  var index = 0
  for (i <- 0 until min_size) {
    for (j <- 0 until n(i); if n(i) > 0) {
      result(index) = elems(i)
      index = index + 1
    }
  }
  result
}

repeat_imperative(Array(2, 3, 4, 5), Array(2, 0, -3, 3))
repeat_imperative(Array(2, 3, 4, 5), Array(2, -3, 3))
repeat_imperative(Array('Q', 'I'), Array(2, 5, 3))
repeat_imperative(Array[Int](), Array(2, 0, -3, 3))
repeat_imperative(Array(1.0, 1.5, 2.0, 3.0), Array(-1, 0, 5))

class BinarySearchTree{
  private class Node(var value: Int, var left: Node = null, var right: Node = null){
    def insert(value: Int): Unit ={
      if(value == this.value)
        return

      val isSmaller = value < this.value
      val child = if(isSmaller) left else right
      if(child == null){
        if(isSmaller) left = new Node(value)
        else right = new Node(value)
      }
      else child.insert(value)
    }

    def print(): Unit ={
      if(left != null) {
        left.print()
        System.out.print(", ")
      }
      System.out.print(value)
      if(right != null) {
        System.out.print(", ")
        right.print()
      }
    }
  }

  private var root: Node = _

  def insert(value: Int): Unit ={
    if(root == null) root = new Node(value)
    else root.insert(value)
  }

  def print(): Unit ={
    if(root != null){
      root.print()
    }
    println()
  }
}

val tree = new BinarySearchTree

tree.print()
tree.insert(2)
tree.insert(5)
tree.insert(-2)
tree.print()
