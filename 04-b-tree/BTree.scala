import scala.reflect.ClassTag
import scala.util.Random

trait Comparator[T] {
  def <>(a: T, b: T): Int
  def isSmaller(a: T, b: T): Boolean = (<>(a, b)) < 0
  def isEq(a: T, b: T): Boolean = (<>(a, b)) == 0
  def isGreater(a: T, b: T): Boolean = (<>(a, b)) > 0
}

object BTree {
  type BranchingFactor = Int

  def empty[K](
      implicit comparator: Comparator[K],
      kClassTag: ClassTag[K],
      branchingFactor: BranchingFactor
  ): BTree[K] = Leaf[K](Array())

  def build[K](values: K*)(
      implicit comparator: Comparator[K],
      kClassTag: ClassTag[K],
      branchingFactor: BranchingFactor
  ) = values.foldLeft(empty[K]) { (t, v) =>
    t.insert(v)
  }

  case class Node[K](
      val keys: Array[K],
      val children: Array[BTree[K]],
      val height: Int
  )(
      implicit val comparator: Comparator[K],
      val kClassTag: ClassTag[K],
      val branchingFactor: BranchingFactor
  ) extends BTree[K] {

    override def toString() =
      "(" + children
        .zip(keys)
        .map(p => p._1 + " <" + p._2 + "> ")
        .mkString("") + children.last.toString + ")"

  }

  case class Leaf[K](
      val keys: Array[K]
  )(
      implicit val comparator: Comparator[K],
      val kClassTag: ClassTag[K],
      val branchingFactor: BranchingFactor
  ) extends BTree[K] {

    def height = 0

    override def toString() =
      "(" + keys.mkString(", ") + ")"

  }
}

sealed trait BTree[K] {
  import BTree._

  implicit def kClassTag: ClassTag[K]
  implicit def comparator: Comparator[K]
  implicit def branchingFactor: BranchingFactor

  def keys: Array[K]
  def height: Int

  /**
    * @return leaf that contains the key and the index of the key
    */
  def search(key: K): (Leaf[K], Int) = this match {
    case Node(keys, children, height) => {
      val index = keys.takeWhile(comparator.isSmaller(_, key)).size
      children(index).search(key)
    }
    case leaf @ Leaf(keys) => {
      val index = keys.takeWhile(comparator.isSmaller(_, key)).size
      (leaf, index)
    }
  }

  def insert(key: K): BTree[K] = {
    if (keys.size == 2 * branchingFactor - 1)
      insertIntoFull(key)
    else
      insertIntoNonFull(key)
  }

  def insertIntoNonFull(key: K): BTree[K] = this match {
    case node @ Node(keys, children, height) => {
      var i = 0
      while (i < keys.size && comparator.isSmaller(keys(i), key)) {
        i += 1
      }

      val newChildren = children.clone()
      newChildren(i) = children(i).insert(key)

      node.copy(children = newChildren)
    }
    case Leaf(keys) => {
      val newKeys = new Array[K](keys.size + 1)

      var i = 0
      while (i < keys.size && comparator.isSmaller(keys(i), key)) {
        newKeys(i) = keys(i)
        i += 1
      }
      newKeys(i) = key
      while (i < keys.size) {
        newKeys(i + 1) = keys(i)
        i += 1
      }

      Leaf(newKeys)
    }
  }

  def insertIntoFull(key: K): BTree[K] =
    Node[K](Array(), Array(this), height + 1)
      .splitChild(0)
      .insert(key)

  /**
    * moves key into the the current node, from a child
    * the child (originally containing the key) split into 2
    * work only on non full nodes
    */
  def splitChild(index: Int): BTree[K] = this match {
    case node @ Node(keys, children, height) => {
      val leftKeys = keys.take(index)
      val rightKeys = keys.takeRight(keys.size - index)
      val leftChildren = children.take(index)
      val rightChildren = children.takeRight(children.size - index - 1)

      val (leftSide, key, rightSide) = children(index).split()

      node.copy(
        keys = leftKeys ++ Array(key) ++ rightKeys,
        children = leftChildren ++ Array(leftSide, rightSide) ++ rightChildren
      )
    }
    case Leaf(keys) =>
      throw new RuntimeException("trying to split child of a Leaf")
  }

  def split(): (BTree[K], K, BTree[K]) = this match {
    case Node(keys, children, height) => {
      val leftKeys = keys.take(branchingFactor - 1)
      val rightKeys = keys.takeRight(branchingFactor - 1)
      val leftChildren = children.take(branchingFactor)
      val rightChildren = children.takeRight(branchingFactor)

      (
        Node(leftKeys, leftChildren, height),
        keys(branchingFactor - 1),
        Node(rightKeys, rightChildren, height)
      )
    }
    case Leaf(keys) => {
      val leftKeys = keys.take(branchingFactor - 1)
      val rightKeys = keys.takeRight(branchingFactor - 1)

      (Leaf(leftKeys), keys(branchingFactor - 1), Leaf(rightKeys))
    }
  }
}

object BTreeTest extends App {
  implicit val intComparator = new Comparator[Int] {
    def <>(a: Int, b: Int) = a - b
  }

  // nodes are never bigger than 2 * 3
  // and never smaller than 3
  implicit val defaultBranchingFactor = 4.asInstanceOf[BTree.BranchingFactor]

  for (i <- 0 to 20) {
    println(BTree.build((0 until i): _*))
  }

  var t = BTree.empty[Int]
  for (i <- 0 to 20) {
    val v = Random.nextInt(200)
    t = t.insert(v)
    println(t)
  }

  println(t.insert(399).search(399))
  println(t.insert(-1).search(-1))

  println(
    BTree.build(42, 300, 175, 94, 1, 950, 250)(
      implicitly[Comparator[Int]],
      implicitly[ClassTag[Int]],
      2.asInstanceOf[BTree.BranchingFactor]
    )
  )
}
