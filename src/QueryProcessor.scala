import scala.annotation.tailrec
import scala.util.matching.Regex

/**
  * Created by fonturacetamum on 04/09/16.
  */
object QueryProcessor {
  def preprocessQuery(query: String): String = {
    query.replace("*", ".*")
  }

  // Simple boolean queries using non-positional index
  def evaluateBooleanQuery(query: String, index: Map[String, List[(Int, String)]]): Set[(Int, String)] = {
    query.split(" ")
      .map(word => index(word).toSet)
      .reduce(_.intersect(_))
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
  def evaluateWildcardQueryKGram(query: String, index: Map[String, List[Int]]) = {//: List[Int] = {
    ???
  }

  def evaluateFuzzyQueryKGram(query: String, index: Map[String, List[(Int, String)]],
                              kGramIndex: Map[String, List[String]]) = {//: List[Int] = {
    val queryKGrams = query
      .split(" ")
      .map(word => KGramGenerator.generate(word))
    queryKGrams
      .map(kGramList =>
        kGramList
          .flatMap(kGram => kGramIndex.get(kGram))
      )
      .map(queryWordResult =>
      queryWordResult
        .foldLeft(Map.empty[String, Int])((wordMap, kGramWordList) =>
          wordMap ++
          kGramWordList.map(kGramWord =>
            if (wordMap contains kGramWord) kGramWord -> (wordMap(kGramWord) + 1)
            else kGramWord -> 1
          ).toMap)
    )
  }
}
