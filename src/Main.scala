/**
  * Created by fonturacetamum on 04/09/16.
  */
object Main extends App{
  val stopWords = IndexBuilder.getStopWords(IndexBuilder.buildInvertedIndex(List("data/retina")), 10)
  stopWords.foreach(println)
}
