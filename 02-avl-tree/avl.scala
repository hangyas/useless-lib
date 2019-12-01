// TODO this file needs much improvement
object avl extends App {
  
  trait Comparator[T] {
    def <>(a: T, b: T): Int
    def isSmaller(a: T, b: T): Boolean = (<>(a, b)) < 0
    def isEq(a: T, b: T): Boolean = (<>(a, b)) == 0
    def isGreater(a: T, b: T): Boolean = (<>(a, b)) > 0
  }

  implicit val intComparator: Comparator[Int] = new Comparator[Int] {
    def <>(a: Int, b: Int) = a - b
  }

  sealed trait Tree[K, V] {
    def get(key: K)(implicit comp: Comparator[K]): Option[V] = this match {
      case Empty() => None
      case node: Node[K, V] if comp.isEq(key, node.key) => Some(node.value)
      case node: Node[K, V] if comp.isSmaller(key, node.key) => node.left.get(key)
      case node: Node[K, V] if comp.isGreater(key, node.key) => node.right.get(key)
    }

    def add(key: K, value: V)(implicit comp: Comparator[K]): Tree[K, V] = (this match {
      case node: Node[K, V] if node.key == key => node.copy(value = value)
      case node: Node[K, V] if node.left == Empty[K, V]() && comp.isSmaller(key, node.key) => node.copy(left = Node(key, value))
      case node: Node[K, V] if node.right == Empty[K, V]() && comp.isGreater(key, node.key) => node.copy(right = Node(key, value))
      case node: Node[K, V] if comp.isSmaller(key, node.key) => node.copy(left = node.left.add(key, value))
      case node: Node[K, V] if comp.isGreater(key, node.key) => node.copy(right = node.right.add(key, value))
      case _ => throw new RuntimeException("Ajaj")
    }).balance()

    def balance(): Tree[K, V] = this match {
      case node: Node[K, V] => {
        val differ = this.difference()
        
        if (differ == 2) {
          // right heavy
          if (node.right.difference() == 1)
            node.rotateLeft()
          else
            node.copy(right = node.right.rotateRight()).rotateLeft()
        } else if (differ == -2) {
          // left heavy
          if (node.left.difference() == -1)
            node.rotateRight()
          else
            node.copy(left = node.left.rotateLeft()).rotateRight()
        } else {
          node.copy(left = node.left.balance(), right = node.right.balance())
        }
      }
      case _: Empty[K, V] => Empty[K, V]()
    }

    def rotateLeft(): Tree[K, V] = this match {
      case Node(xk, xv, t1, Node(zk, zv, t2, t3)) => Node(zk, zv, Node(xk, xv, t1, t2), t3)
      case Empty() => this
      case _ => throw new RuntimeException("oh no")
    }

    def rotateRight(): Tree[K, V] = this match {
      case Node(yk, yv, Node(xk, xv, t1, t2), t3) => Node(xk, xv, t1, Node(yk, yv, t2, t3))
      case Empty() => this
      // case node: Node[K, V] => throw new RuntimeException("oh no" + node.right.height())
      case _ => throw new RuntimeException("oh no")
    }

    def difference(): Int = this match {
      case node: Node[K, V] => {
        val leftHeight = node.left.height() // TODO lazy val
        val rightHeight = node.right.height()
        
        return rightHeight - leftHeight
      } 
      case Empty() => 0
    }

    def height(): Int = this match {
      case Empty() => 0
      case Node(_, _, left, right) => Math.max(left.height(), right.height()) + 1
    }
  }

  object Tree {
    def node[K, V](key: K, value: V, left: Tree[K, V], right: Tree[K, V]): Tree[K, V] = Node(key, value, left, right)
    def empty[K, V]() = Empty
  }

  case class Node[K, V](
    key: K,
    value: V,
    left: Tree[K, V] = Empty[K, V](),
    right: Tree[K, V] = Empty[K, V]()
  ) extends Tree[K, V] {
    def copy(key: K = this.key, value: V = this.value, left: Tree[K, V] = this.left, right: Tree[K, V] = this.right): Node[K, V] = 
      Node(key, value, left, right)
  }

  case class Empty[K, V]() extends Tree[K, V]

  val test = Node(5, 15,
    Node(2, 12, Empty(), Empty()),
    Node(7, 17,
      Node(6, 16, Empty(), Empty()),
      Empty()
    )
  )

  println(test.get(2))
  println(test.get(1))
  println(test.get(5))
  println(test.get(7))

  println(test)
  println(test.rotateRight())
  println(test.rotateLeft())
  println(test.rotateLeft().rotateRight() == test)

  println(Node(2, 12).add(3, 13).add(1, 11).height()) // TODO implement some fancy printing
  val tree = Node(1, 1).add(2, 2).add(3, 3).add(4, 4).add(5, 5).add(6, 6)
  println(tree)
  for (i <- 1 to 6) {
    println(tree.get(i) == Some(i))
  }

}
