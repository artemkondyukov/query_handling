import scala.language.postfixOps

/**
  * Created by fonturacetamum on 04/09/16.
  */

object IndexBuilder {
  def cleanString(string: String): String = {
    // Remove all special characters
    string.replaceAll("[^A-Za-z0-9 -]", "").toLowerCase
  }

  def buildInvertedIndex(corpusFilenames: List[String]): Map[String, (Int, String)] = {
    val documentMap: Map[String, String] =
      corpusFilenames.map(filename =>
        filename -> cleanString(scala.io.Source.fromFile(filename).mkString)).toMap
    documentMap.map { case (documentName, text) =>
      text.split(" ")
        .map(word => Map(word -> (1, documentName)))
        .reduce((leftMap, rightMap) =>
          leftMap ++ rightMap.filter(rTuple => !leftMap.contains(rTuple._1))
                  ++ rightMap.filter(rTuple => leftMap.contains(rTuple._1))
          .map(rTuple => rTuple._1 -> (leftMap(rTuple._1)._1 + rTuple._2._1, rTuple._2._2))
        )
    } head
  }

  def getStopWords(index: Map[String, (Int, String)], num: Int) = {
    scala.util.Sorting.stableSort(index.toList,
      (left: (String, (Int, String)), right: (String, (Int, String))) =>
        left._2._1 > right._2._1).take(10)
  }
}