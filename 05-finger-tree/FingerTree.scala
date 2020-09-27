/**
  * Based on the excellent paper "Finger Trees Explained Anew, and Slightly Simplified (Functional Pearl)"
  * by Koen Claessen https://doi.org/10.1145/3406088.3409026
  */
object FingerTree extends App {

  sealed trait FTSeq[T]
  case class FTNil[T]() extends FTSeq[T]
  case class FTUnit[T](v: T) extends FTSeq[T]
  case class FTMore[T](
      left: FTSome[T],
      middle: FTSeq[Tuple[T]], // stores elements in blocks twice as big as the current level -> logn
      right: FTSome[T]
  ) extends FTSeq[T]

  sealed trait FTSome[T]
  case class FTOne[T](a: T) extends FTSome[T]
  case class FTTwo[T](a: T, b: T) extends FTSome[T]
  case class FTThree[T](a: T, b: T, c: T) extends FTSome[T]

  sealed trait Tuple[T]
  case class Pair[T](a: T, b: T) extends Tuple[T]
  case class Triple[T](a: T, b: T, c: T) extends Tuple[T]

  def head[T](seq: FTSeq[T]): T = seq match {
    case FTUnit(v)                      => v
    case FTMore(FTOne(v), _, _)         => v
    case FTMore(FTTwo(v, _), _, _)      => v
    case FTMore(FTThree(v, _, _), _, _) => v
  }

  def prepend[T](v: T, seq: FTSeq[T]): FTSeq[T] = seq match {
    case FTNil()                            => FTUnit(v)
    case FTUnit(a)                          => FTMore(FTOne(v), FTNil(), FTOne(a))
    case FTMore(FTOne(left), middle, right) => FTMore(FTTwo(v, left), middle, right)
    // three is used so head tail is constant time (with two it would have to recurse)
    case FTMore(FTTwo(a, b), middle, right) => FTMore(FTThree(v, a, b), middle, right)
    // a and b paired up and prepended to middle
    case FTMore(FTThree(a, b, c), middle, right) =>
      FTMore(FTTwo(v, a), prepend(Pair(b, c), middle), right)
  }

  def append[T](seq: FTSeq[T], v: T): FTSeq[T] = seq match {
    case FTNil()                            => FTUnit(v)
    case FTUnit(a)                          => FTMore(FTOne(a), FTNil(), FTOne(v))
    case FTMore(left, middle, FTOne(right)) => FTMore(left, middle, FTTwo(right, v))
    // a and b paired up and prepended to middle
    case FTMore(left, middle, FTTwo(a, b)) => FTMore(left, append(middle, Pair(a, b)), FTOne(v))
  }

  def tail[T](seq: FTSeq[T]): FTSeq[T] = seq match {
    case FTUnit(v)                               => FTNil()
    case FTMore(FTThree(_, a, b), middle, right) => FTMore(FTTwo(a, b), middle, right)
    case FTMore(FTTwo(_, a), middle, right)      => FTMore(FTOne(a), middle, right)
    case FTMore(FTOne(_), middle, right)         => more0(middle, right)

  }

  private def more0[T](seq: FTSeq[Tuple[T]], a: FTSome[T]): FTSeq[T] =
    (seq, a) match {
      case (FTNil(), FTOne(v))         => FTUnit(v)
      case (FTNil(), FTTwo(a, b))      => FTMore(FTOne(a), FTNil(), FTOne(b))
      case (FTNil(), FTThree(a, b, c)) => FTMore(FTOne(a), FTNil(), FTTwo(b, c))
      case (s, d) => {
        head(s) match {
          case Pair(a, b)      => FTMore(FTTwo(a, b), tail(s), d)
          case Triple(a, b, c) => FTMore(FTOne(a), chopHead(s), d)
        }
      }
    }

  /**
    * chops the head Triple into Pair
    */
  private def chopHead[T](seq: FTSeq[Tuple[T]]): FTSeq[Tuple[T]] = seq match {
    case FTUnit(Triple(_, b, c))                       => FTUnit(Pair(b, c))
    case FTMore(FTOne(Triple(_, b, c)), middle, right) => FTMore(FTOne(Pair(b, c)), middle, right)
    case FTMore(FTTwo(Triple(_, b, c), d), middle, right) =>
      FTMore(FTTwo(Pair(b, c), d), middle, right)
    case FTMore(FTThree(Triple(_, b, c), d, e), middle, right) =>
      FTMore(FTThree(Pair(b, c), d, e), middle, right)
    case x => x
  }

  private def toList[T](some: FTSome[T]): List[T] = some match {
    case FTOne(a)         => List(a)
    case FTTwo(a, b)      => List(a, b)
    case FTThree(a, b, c) => List(a, b, c)
  }

  /**
    * converts a longer list of elements into a shorter list of tuples
    */
  private def listToTuples[T](list: List[T]): List[Tuple[T]] = list match {
    case Nil                     => Nil
    case a :: b :: Nil           => Pair(a, b) :: Nil
    case a :: b :: c :: d :: Nil => Pair(a, b) :: Pair(c, d) :: Nil
    case a :: b :: c :: tail     => Triple(a, b, c) :: listToTuples(tail)
  }

  /**
    * Useful helper function for `++`
    * @param left left of `++`
    * @param middle stick between left and right, max size = 2
    * @param right right of `++`
    */
  private def glue[T](left: FTSeq[T], middle: List[T], right: FTSeq[T]): FTSeq[T] =
    (left, middle, right) match {
      case (FTNil(), middle, right)   => middle.foldRight(right)(prepend)
      case (left, middle, FTNil())    => middle.foldLeft(left)(append)
      case (FTUnit(a), middle, right) => (a :: middle).foldRight(right)(prepend)
      case (left, middle, FTUnit(a))  => (middle ::: List(a)).foldLeft(left)(append)
      case (FTMore(l1, m1, r1), middle, FTMore(l2, m2, r2)) =>
        FTMore(
          l1,
          glue(m1, listToTuples(toList(r1) ::: middle ::: toList(l2)), m2),
          r2
        )
    }

  // -- examples --

  def empty[T]: FTSeq[T] = FTNil[T]()

  def build[T](original: Seq[T]) = original.foldLeft(empty[T]) {
    case (seq, v) => { prepend(v, seq) }
  }

  (0 to 10).map(i => build(0 until i)).foreach(println)
  println(head(build(0 until 10)))
  println(tail(build(0 until 10)))

}
