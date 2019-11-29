import scala.collection.mutable


object btw extends App {

  /**
    * last column from sorted allRotations
    *
    * $ = EOF
    */
  def bwt(input: String): String =
    allRotations(input + "$").sorted.map(_.last).mkString

  /**
    * banana$ ->
    *
    * banana$
    * $banana
    * a$banan
    * na$bana
    * ana$ban
    * nana$ba
    * anana$b
    *
    * allRotations("banana$").foreach(println(_))
    */
  def allRotations(input: String): List[String] = {
    var l = input.toList

    var r = List[String]()
    (0 until input.size).foreach { i =>
      l = l.tail ::: l.head :: Nil
      r = l.mkString :: r
    }

    return r
  }

  def reverseBwt(input: String): String = {
    val pairs = mutable.ArrayBuffer(input.sorted.zip(input): _*)

    val result = new StringBuilder()
    var c = '$'
    while (pairs.nonEmpty) {
      val i = pairs.lastIndexWhere(_._2 == c)
      c = pairs(i)._1 // original character to the right (because of cyclic shifting)
      pairs.remove(i)

      result += c
    }

    return result.mkString
  }

  println(bwt("banana")) // annb$aa
  println(reverseBwt("annb$aa").dropRight(1)) // banana
  
}
