/**
  * Created by fonturacetamum on 04/09/16.
  */
object Main extends App{
  val index = IndexBuilder.buildInvertedIndex(List("data/doc1", "data/doc2", "data/doc3", "data/doc4", "data/doc5"))
  val kGramIndex = IndexBuilder.buildKGramIndex(index.keys.toList)
  val permutationIndex = IndexBuilder.buildPermutationIndex(index.keys.toList)

  println()
  println("KGram index: ")
  kGramIndex foreach println

  println()
  println("Permutation index: ")
  permutationIndex foreach println

  println()
  println("\"th* *d*a*y*\" query using kgrams: ")
  val resultKGram = QueryProcessor.evaluateWildcardQueryKGram("th* *d*a*y*", index, kGramIndex)
  resultKGram foreach println

  println()
  println("\"f*m d*y\" query using permutations: ")
  val resultPermutations = QueryProcessor.evaluateWildcardQueryPermutation("f*m d*y", index, permutationIndex)
  resultPermutations foreach println
}
