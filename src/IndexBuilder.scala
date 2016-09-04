import scala.language.postfixOps

/**
  * Created by fonturacetamum on 04/09/16.
  */

object IndexBuilder {
  def cleanString(string: String): String = {
    // Remove all special characters
    string.replaceAll("[^A-Za-z0-9 -]", "")
  }

  def buildInvertedIndex(corpusFilenames: List[String]): Map[String, Int] = {
    val documentMap: Map[String, String] =
      corpusFilenames.map(filename =>
        filename -> cleanString(scala.io.Source.fromFile(filename).mkString)).toMap
    documentMap.map { case (documentName, text) =>
      text.split(" ")
        .map(word => Map(word -> 1))
        .reduce((leftMap, rightMap) =>
          leftMap ++ rightMap.filter(rTuple => !leftMap.contains(rTuple._1))
                  ++ rightMap.filter(rTuple => leftMap.contains(rTuple._1))
          .map(rTuple => rTuple._1 -> (leftMap(rTuple._1) + rTuple._2))
        )
    } head
//    Map("" -> Nil)
  }
}