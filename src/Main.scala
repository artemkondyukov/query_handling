/**
  * Created by fonturacetamum on 04/09/16.
  */
object Main extends App{
//  val stopWords = IndexBuilder.getStopWords(IndexBuilder.buildInvertedIndex(List("data/retina")), 10)
  val stopWords = IndexBuilder.getStopWords(IndexBuilder.buildInvertedIndex(List("data/doc1", "data/doc2")), 10)
  stopWords.foreach(println)
//  println(IndexBuilder.buildInvertedIndex(List("data/doc1", "data/doc2")))
}
