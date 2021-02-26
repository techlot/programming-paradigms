sealed trait Tree[T]
case class Leaf[T](value: T) extends Tree[T]
case class SingularBranch[T, O](func: T => O, child: Tree[T]) extends Tree[O]
case class DoubledBranch[T, A, O](func: (T, A) => O, left: Tree[T], right: Tree[A])  extends Tree[O]

def evaluate[T](tree: Tree[T]): T = tree match {
  case Leaf(value) => value
  case SingularBranch(function, child) => function(evaluate(child))
  case DoubledBranch(function, left, right) => function(evaluate(left), evaluate(right))
}

val function_1 = (x: Double, y: Double) => x + y
val right_1 = (x: Double) => -x
val left_1 = (x: Int) => x.toDouble

val tree1 = DoubledBranch(function_1, SingularBranch(right_1, Leaf(1.0)), SingularBranch(left_1, Leaf(5)))

evaluate(tree1)
