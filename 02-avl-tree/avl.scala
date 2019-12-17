// TODO this file needs much improvement
object avl extends App {

  trait Comparator[T] {
    def <>(a: T, b: T): Int
    def isSmaller(a: T, b: T): Boolean = (<>(a, b)) < 0
    def isEq(a: T, b: T): Boolean = (<>(a, b)) == 0
    def isGreater(a: T, b: T): Boolean = (<>(a, b)) > 0
  }

  implicit val intComparator = new Comparator[Int] {
    def <>(a: Int, b: Int) = a - b
  }

  sealed trait Tree[K, V] {
    // we need to balance when one or more child is changed in a copy
    // it will only recalculate the depth of the changed element (because then it's stored in lazy val)
    // TODO find way to elegantly store isBalanced as well

    def get(key: K)(implicit comp: Comparator[K]): Option[V] = this match {
      case Empty() => None
      case node: Node[K, V] => {
        if (comp.isSmaller(key, node.key)) {
          node.left.get(key)
        } else if (comp.isGreater(key, node.key)) {
          node.right.get(key)
        } else {
          Some(node.value)
        }
      }
    }

    def add(key: K, value: V)(implicit comp: Comparator[K]): Tree[K, V] =
      this match {
        case Empty() => Node(key, value, Empty(), Empty())
        case node: Node[K, V] => {
          if (comp.isSmaller(key, node.key)) {
            node.copy(left = node.left.add(key, value)).balanced
          } else if (comp.isGreater(key, node.key)) {
            node.copy(right = node.right.add(key, value)).balanced
          } else {
            node.copy(value = value).balanced
          }
        }
      }

    def delete(key: K)(implicit comp: Comparator[K]): Tree[K, V] = this match {
      case Empty() => Empty() // no such element 
      case node: Node[K, V] => {
        if (comp.isSmaller(key, node.key)) {
          node.copy(left = node.left.delete(key)).balanced
        } else if (comp.isGreater(key, node.key)) {
          node.copy(right = node.right.delete(key)).balanced
        } else {
          // equals
          node match {
            case Node(_, _, Empty(), Empty()) => Empty()
            case Node(_, _, left, Empty()) => left
            case Node(_, _, Empty(), right) => right
            case Node(_, _, left, right) => right.findLeftMost().copy(left = left, right = right.deleteLeftMost()).balanced
          }
        }
      }
    }

    // TODO try to do findLeftMost and deleteLeftMost in one call:
    //    pass the parent tree to the parameters and add the right children in every step

    private def findLeftMost(): Node[K, V] = this match {
      case node @ Node(_, _ , Empty(), _) => node
      case node @ Node(_, _ , left, _) => left.findLeftMost()
    }

    private def deleteLeftMost(): Tree[K, V] = this match {
      case node @ Node(_, _, Empty(), right) => right
      case node @ Node(_, _ , left, _) => node.copy(left = left.deleteLeftMost())
    }

    def balanced: Tree[K, V] = this match {
      case node @ Node(_, _, _, right)
          if node.heightDiff == 2 && right.heightDiff == 1 =>
        node.rotatedLeft
      case node @ Node(_, _, _, right) if node.heightDiff == 2 =>
        node.copy(right = right.rotatedRight).rotatedLeft
      case node @ Node(_, _, left, _)
          if node.heightDiff == -2 && left.heightDiff == -1 =>
        node.rotatedRight
      case node @ Node(_, _, left, _) if node.heightDiff == -2 =>
        node.copy(left = left.rotatedLeft).rotatedRight
      case node @ Node(_, _, left, right) =>
        node.copy(left = left.balanced, right = right.balanced)
      case Empty() => Empty()
    }

    def rotatedLeft: Tree[K, V] = this match {
      case Node(xk, xv, t1, Node(zk, zv, t2, t3)) =>
        Node(zk, zv, Node(xk, xv, t1, t2), t3)
      case Empty() => this
      case _ => throw new RuntimeException("wrong use of rotateLeft")
    }

    def rotatedRight: Tree[K, V] = this match {
      case Node(yk, yv, Node(xk, xv, t1, t2), t3) =>
        Node(xk, xv, t1, Node(yk, yv, t2, t3))
      case Empty() => this
      case _ => throw new RuntimeException("wrong use of rotateRight")
    }

    lazy val heightDiff: Int = this match {
      case Node(_, _, left, right) => right.height - left.height
      case Empty()                 => 0
    }

    lazy val height: Int = this match {
      case Node(_, _, left, right) => Math.max(left.height, right.height) + 1
      case Empty()                 => 0
    }
  }

  object Tree {
    def create[K, V](pairs: (K, V)*)(implicit comp: Comparator[K]): Tree[K, V] = {
      var r: Tree[K, V] = Node(pairs.head._1, pairs.head._2)
      return pairs.tail.foldLeft(r){(a: Tree[K, V], b: (K, V)) => a.add(b._1, b._2)(comp)}
      // return null
    }
  }

  case class Node[K, V](
      key: K,
      value: V,
      left: Tree[K, V] = Empty[K, V](),
      right: Tree[K, V] = Empty[K, V]()
  ) extends Tree[K, V]

  case class Empty[K, V]() extends Tree[K, V]

  val test = Node(
    5,
    15,
    Node(2, 12, Empty(), Empty()),
    Node(7, 17, Node(6, 16, Empty(), Empty()), Empty())
  )

  println(test.get(2))
  println(test.get(1))
  println(test.get(5))
  println(test.get(7))

  println(test)
  println(test.rotatedLeft)
  println(test.rotatedRight)
  println(test.rotatedLeft.rotatedRight == test)

  println(Node(2, 12).add(3, 13).add(1, 11).height) // TODO implement some fancy printing
  val tree = Node(1, 1).add(2, 2).add(3, 3).add(4, 4).add(5, 5).add(6, 6)
  println(tree)
  println(tree.delete(1))
  for (i <- 1 to 6) {
    println(tree.get(i) == Some(i))
  }

  println(Node(2, 2).add(1, 1).add(3, 3).delete(2))
  println(Node(2, 2).add(1, 1).add(3, 3).delete(2).delete(3))
  println(Node(1, 1, Node(2, 2, Node(3, 3, Node(4, 4), Empty()), Empty()), Empty()))
  println(Node(1, 1, Node(2, 2, Node(3, 3, Node(4, 4), Empty()), Empty()), Empty().balanced))

  println(tree == Tree.create(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5, 6 -> 6))
}
