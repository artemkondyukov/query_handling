import scala.language.postfixOps

/**
  * Created by fonturacetamum on 04/09/16.
  */

object IndexBuilder {
  def mergeMaps[A, B](mergeFunction: (B, B) => B)(leftMap: Map[A, B], rightMap: Map[A, B]): Map[A, B] = {
    leftMap ++
      rightMap.filter(rTuple => !leftMap.contains(rTuple._1)) ++
      rightMap.filter(rTuple => leftMap.contains(rTuple._1))
        .map(rTuple => rTuple._1 -> mergeFunction(leftMap(rTuple._1), rTuple._2))
  }

  def mergeMapsListConcatValues[A] = {
    mergeMaps((leftList: List[A], rightList: List[A]) =>
      leftList ::: rightList) (
      _: Map[String, List[A]],
      _: Map[String, List[A]]
    )
  }

  def cleanString(string: String): String = {
    // Remove all special characters
    string.replaceAll("[^A-Za-z0-9 -]", "").toLowerCase
  }

  def buildInvertedIndex(corpusFilenames: List[String]): Map[String, List[(Int, String)]] = {
    def mergeMapsInner = {
      mergeMaps((lTuple: List[(Int, String)], rTuple: List[(Int, String)]) =>
        List((lTuple.head._1 + rTuple.head._1, rTuple.head._2))) (
          _: Map[String, List[(Int, String)]],
          _: Map[String, List[(Int, String)]]
      )
    }

    val documentMap =
      corpusFilenames.map(filename =>
        filename -> cleanString(scala.io.Source.fromFile(filename).mkString)).toMap
    documentMap.map { case (documentName, text) =>
      text.split(" ")
        .map(word => Map(word -> List((1, documentName))))
        .reduce(
          mergeMapsInner
        )
    }
    .reduce((lMap: Map[String, List[(Int, String)]], rMap: Map[String, List[(Int, String)]]) =>
      mergeMapsListConcatValues(lMap, rMap)
    )
  }

  def buildKGramIndex(vocabulary: List[String], k: Int): Map[String, List[String]] = {
    vocabulary
      .map(word => KGramGenerator.generate(word, k).map(kgram => kgram -> List(word)).toMap)
      .reduce((lMap: Map[String, List[String]], rMap: Map[String, List[String]]) =>
        mergeMapsListConcatValues(lMap, rMap)
      )
  }

  def buildPermutationIndex(vocabulary: List[String]): Map[String, String] = {
    vocabulary
      .map(word => PermutermGenerator.generate(word).map(perm => perm -> word).toMap)
      .reduce(_ ++ _)
  }

  def getStopWords(index: Map[String, List[(Int, String)]], num: Int) = {
    val sumCounts: Map[String, Int] = index.mapValues(list => list.foldLeft(0)((sum, rTuple) => sum + rTuple._1))
    scala.util.Sorting.stableSort(sumCounts.toList,
      (left: (String, Int), right: (String, Int)) => left._2 > right._2).take(10)
  }
}