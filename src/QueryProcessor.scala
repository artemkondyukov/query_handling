import scala.annotation.tailrec
import scala.util.matching.Regex

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
  def simpleIntersect = intersect(_: Seq[Int], _: Seq[Int], (l: Int, r: Int) => l == r)

  def preprocessQuery(query: String): String = {
    query.replace("*", ".*")
  }

  // Simple boolean queries using non-positional index
  def evaluateBooleanQuery(query: String, index: Map[String, List[Int]]): List[Int] = {
    query.split(" ")
      .map(word => index(word))
      .reduce(simpleIntersect)
      .toList
  }

  // A very dull implementation, linear in size of vocabulary
  def evaluateWildcardQueryLinear(query: String, index: Map[String, List[(Int, String)]]) : Set[(Int, String)] = {
    val words = preprocessQuery(query).split(" ").map(_.r)
    index
      .toList
      .flatMap {
        pList => {
          val matchingRegexes = words
            .map(word => if (word.findFirstIn(pList._1).isDefined) Some(word) else None)
            .collect { case Some(x) => x }
          if (matchingRegexes.nonEmpty) Some(pList._1 -> matchingRegexes.toList)
          else None
        }
      }
      .foldLeft(Map.empty[Regex, List[String]])((l, r: (String, List[Regex])) =>
        l ++
        r._2
          .map ( regex =>
            if (l contains regex) regex -> (r._1 :: l(regex))
            else regex -> (r._1 :: Nil)
          )
          .toMap
      )
      .values
      .map( wordList =>
        wordList
          .map(word => index(word).toSet)
          .reduce(_.union(_))
      )
      .reduce(_.intersect(_))
  }

  // A kgram based implementation
  def evaluateWildcardQueryKGram(query: String, index: Map[String, List[Int]]): List[Int] = {
    Nil
  }

  def evaluateFuzzyQuery(query: String, index: Map[String, List[Int]]): List[Int] = {
    Nil
  }
}
