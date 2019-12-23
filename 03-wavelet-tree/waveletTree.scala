object waveletTree extends App {

  type BitVector = Array[Int]

  /**
    * The performance of the algorithm stands or falls on this data structure
    * O(1) rank and select exist, but these aren't those
    */
  class StupidBitVectorImpl(val array: Array[Int]) {

    // def select(i: Int): Int = null

    def rank(i: Int): Int = array.take(i).count(_ == 1)

    def rankZero(i: Int): Int = array.take(i).count(_ == 0)

    def ones(i: Int, length: Int) = array.drop(i).take(length).count(_ == 1)

    def zeros(i: Int, length: Int) = array.drop(i).take(length).count(_ == 0)

  }

  implicit def arrayToBitVector(array: Array[Int]) =
    new StupidBitVectorImpl(array)

  sealed trait Tree[T] {

    def get(i: Int): T = this match {
      case Node(bitvector, left, right) =>
        bitvector(i) match {
          case 0 => left.get(bitvector.rankZero(i))
          case 1 => right.get(bitvector.rank(i))
        }
      case Leaf(data) => data
    }

    def range(i: Int, length: Int): List[T] = this match {
      case _ if length < 1 => List[T]()
      case Node(bitvector, left, right) => {
        val leftRange =
          left.range(bitvector.rankZero(i), bitvector.zeros(i, length))
        val rightRange =
          right.range(bitvector.rank(i), bitvector.ones(i, length))
        leftRange ::: rightRange
      }
      case Leaf(data) => data :: Nil
    }

  }

  case class Node[T](val bitvector: BitVector, left: Tree[T], right: Tree[T])
      extends Tree[T]
  case class Leaf[T](val data: T) extends Tree[T]

  def buildNaiveTree[T](alphabet: List[T], data: List[T]): Tree[T] = {
    if (alphabet.length == 1)
      return Leaf(alphabet.head)

    val alphabetIndex = alphabet.zipWithIndex.map {
      case (c, i) => (c, if (i < alphabet.length / 2) 0 else 1)
    }.toMap
    val bitvector = data.map(c => alphabetIndex(c)).toArray[Int]

    val (alphabetLeft, alphabetRight) = alphabet.splitAt(alphabet.length / 2)
    val dataLeft = data.filter(c => alphabetIndex(c) == 0)
    val dataRight = data.filter(c => alphabetIndex(c) == 1)

    return Node(
      bitvector,
      buildNaiveTree(alphabetLeft, dataLeft),
      buildNaiveTree(alphabetRight, dataRight)
    )
  }

  // TODO build huffmann shaped

  val t = buildNaiveTree(
    (0 until 10).toList,
    List(2, 7, 7, 6, 4, 8, 3, 6, 4, 4, 5, 6, 3, 4, 5, 8, 9, 5, 4, 2)
  )
  println(t.get(0))
  println(t.get(2))
  println(t.range(1, 6))

}
