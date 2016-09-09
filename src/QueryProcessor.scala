import scala.collection.immutable.TreeMap
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
          .map ( regex => regex -> (r._1 :: l.getOrElse(regex, Nil)) )
          .toMap
      )
      .values
      .map( wordList =>
        wordList
          .map(word => index(word).toSet)
          .reduce(_ union _)
      )
      .reduce(_ intersect _)
  }

  def evaluateWildcardQueryKGram(query: String, index: Map[String, List[(Int, String)]],
                                 kGramIndex: Map[String, List[String]]) = {//: List[Int] = {
    val words = preprocessQuery(query).split(" ").map(_.r)
    val queryKGrams: List[List[String]] = query
      .split(" ")
      .map(word => KGramGenerator.generate(word).filter(!_.contains('*')))
      .toList
    queryKGrams foreach println
    val kGramsMatchingWordsLists: List[List[List[String]]] = queryKGrams
      .map(kGramsList => {
        if (kGramsList.nonEmpty)
          kGramsList
            .map(kGram => kGramIndex.get(kGram))
            .collect {
              case Some(x) => x
              case None => Nil
            }
        else
          List(index.keys.toList)
      }
      )
//    kGramsMatchingWordsLists foreach println
    if (kGramsMatchingWordsLists.exists(_.exists(_.isEmpty))) Nil else
    kGramsMatchingWordsLists
      .zip(words)
      .map(wLists =>
        wLists._1
          .map(wList => wList.filter(word => wLists._2.findFirstIn(word).isDefined))
          .reduce(_ intersect _)
      )
      .map(pList =>
        pList
          .map(word =>
            index(word)
            .map(_._2)
            .toSet)
          .reduce(_ union _)
      )
      .reduce(_ intersect _)
  }

  def evaluateWildcardQueryPermutation(query: String, index: Map[String, List[(Int, String)]],
                                    permutationIndex: TreeMap[String, String]) = {
    val queryWords = preprocessQuery(query).split(" ")
    queryWords
      .map(word => {
        val asteriskPositions = """\.\*""".r.findAllMatchIn(word).toList
        val permutedWord = word.takeRight(word.length - 2 - asteriskPositions.last.start) + "$" +
          word.take(asteriskPositions.head.start)
        val matchWords = permutationIndex
          .range(permutedWord, permutedWord.init + (permutedWord.last + 1).toChar)
          .values
          .filter(wordInRange => word.r.findFirstIn(wordInRange).isDefined)
        if (matchWords.nonEmpty)
          matchWords
            .map(matchWord => index(matchWord).toSet)
          .reduce(_ union _)
        else
          Set.empty[(Int, String)]
      }
      )
      .reduce(_ intersect _)
  }

  private def evaluateFuzzyQueryKGram(query: String, index: Map[String, List[(Int, String)]],
                              kGramIndex: Map[String, List[String]],
                              filterFunction: List[(String, Int)] => List[(String, Int)]) = {//: List[Int] = {
    val queryKGrams = query
      .split(" ")
      .map(word => KGramGenerator.generate(word))
    queryKGrams
      .map(kGramList =>
        kGramList
          .flatMap(kGram => kGramIndex.get(kGram))
      )
      .map(queryWordResult => {
        val wordsUnfiltered = queryWordResult
          .foldLeft(Map.empty[String, Int])((wordMap, kGramWordList) =>
            wordMap ++
              kGramWordList.map(kGramWord =>
                kGramWord -> (wordMap.getOrElse(kGramWord, 0) + 1)
              ).toMap)
          .toList
        filterFunction(wordsUnfiltered)
          .map(wordCountTuple =>
            index(wordCountTuple._1).map(_._2).toSet
          )
      }
        .reduce(_ union _)
      )
    .reduce(_ intersect _)
  }

  def evaluateFuzzyQueryKGramEditDistance(query: String, index: Map[String, List[(Int, String)]],
                                         kGramIndex: Map[String, List[String]],
                                         mostProximateKGram: Int = 10,
                                         mostProximateEditDistance: Int = 3) = {
    def filterFunction(wordSimilarities: List[(String, Int)]): List[(String, Int)] = {
      query
        .split(" ")
        .zip(wordSimilarities)
        .sortBy(_._2._2)
        .take(mostProximateKGram)
        .map(wordSim =>
          wordSim._2._1 -> Levenshtein.distance(wordSim._1, wordSim._2._1)
        )
        .sortBy(_._2)
        .toList
        .take(mostProximateEditDistance)
    }
    evaluateFuzzyQueryKGram(query, index, kGramIndex, filterFunction)
  }

  def evaluateFuzzyQueryKGramOnly(query: String, index: Map[String, List[(Int, String)]],
                                    kGramIndex: Map[String, List[String]],
                                    mostProximateKGram: Int = 5) = {
    def filterFunction(wordSimilarities: List[(String, Int)]): List[(String, Int)] = {
      wordSimilarities
        .sortBy(_._2)
        .take(mostProximateKGram)
    }
    evaluateFuzzyQueryKGram(query, index, kGramIndex, filterFunction)
  }
}
