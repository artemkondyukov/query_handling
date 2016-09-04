import scala.annotation.tailrec

/**
  * Created by fonturacetamum on 04/09/16.
  */
object QueryProcessor {
  @tailrec final def intersect(p1: Seq[Int], p2: Seq[Int],
                               isCloseEnough: (Int, Int) => Boolean,
                               accumulator: Seq[Int] = Nil
                              ): Seq[Int] = {
    (p1, p2) match {
      case (Nil, Nil) => accumulator
      case (p, Nil) => accumulator
      case (Nil, p) => accumulator
      case (p1_head :: p1_tail, p2_head :: p2_tail) =>
        if (isCloseEnough(p1_head, p2_head))
          intersect(p1_tail, p2_tail, isCloseEnough, accumulator :+ p1_head)
        else if (p1_head < p2_head)
          intersect(p1_tail, p2, isCloseEnough, accumulator)
        else
          intersect(p1, p2_tail, isCloseEnough, accumulator)
    }
  }

  // Simple boolean queries using non-positional index
  def evaluateBooleanQuery(query: String, index: Map[String, List[Int]]): List[Int] = {
    def simpleIntersect = intersect(_: Seq[Int], _: Seq[Int], (l: Int, r: Int) => l == r)
    query.split(" ")
      .map(word => index(word))
      .reduce(simpleIntersect)
  }.toList

  def generateKGrams(query: String, k: Int): List[String] = {
    query.split(" ")
      .map(word => (1 to word.length)
                    .toList
                    .map(index =>
                      ("$" + word + "$").takeRight(word.length - index + 3).take(k)))
      .reduce(_ ::: _)
  }

  def generatePermuterm(query: String): List[String] = {
    query.split(" ")
      .map(word => (1 to word.length + 1)
                    .toList
                    .map(index =>
                      (word + "$").takeRight(word.length + 2 - index) + (word + "$").take(index - 1)
                    ))
      .reduce(_ ::: _)
  }
}
