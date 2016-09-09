import java.io.{InputStreamReader, PrintWriter}
import java.util.{NoSuchElementException, Scanner}

import scala.reflect.io.Directory

/**
  * Created by fonturacetamum on 04/09/16.
  */

object Main extends App{
  val d = Directory("./data/journal_2")
  val files = d.list.filter(_.isFile).map(_.toString).toList
  val index = IndexBuilder.buildInvertedIndex(files)
  val kGramIndex = IndexBuilder.buildKGramIndex(index.keys.toList)
  val permutationIndex = IndexBuilder.buildPermutationIndex(index.keys.toList)

  println("Index constructed")

//  println()
//  println("Index: ")
//  val indexPW = new PrintWriter("indices/vocabulary")
//  index.foreach(pList => indexPW.println(pList._1))

  val scanner = new Scanner(new InputStreamReader(System.in))
  def prompt(scanner: Scanner): Unit = {
    val inputString = scanner.nextLine()
    inputString match {
      case "end" =>
      case _ =>
        val wildCardRegex = """.*\*.*""".r
        val queryCharSeq: CharSequence = inputString
        queryCharSeq match {
          case wildCardRegex() => {
            val startTimeLinear = System.nanoTime()
            val linearResults = QueryProcessor.evaluateWildcardQueryLinear(inputString, index)
            println("Linear method. Time elapsed: " +
              (System.nanoTime() - startTimeLinear) / 1000 + " usec. " +
              linearResults.size + " results were found.")

            val startTimeKGram = System.nanoTime()
            val kGramResults = QueryProcessor.evaluateWildcardQueryKGram(inputString, index, kGramIndex)
            println("KGram method. Time elapsed: " +
              (System.nanoTime() - startTimeKGram) / 1000 + " usec. " +
              kGramResults.size + " results were found.")

            val startTimePermutation = System.nanoTime()
            val permuattionResults = QueryProcessor.evaluateWildcardQueryPermutation(inputString, index, permutationIndex)
            println("Permutation method. Time elapsed: " +
              (System.nanoTime() - startTimePermutation) / 1000 + " usec. " +
              permuattionResults.size + " results were found.")
          }
          case _ => {
            val startTimeBoolean = System.nanoTime()
            val booleanResults = QueryProcessor.evaluateBooleanQuery(inputString, index)
            println("Boolean method. Time elapsed: " +
              (System.nanoTime() - startTimeBoolean) / 1000 + " usec. " +
              booleanResults.size + " results were found.")

            val startTimeFuzzyKGED = System.nanoTime()
            val fuzzyKGEDResults = QueryProcessor.evaluateFuzzyQueryKGramEditDistance(inputString, index, kGramIndex)
            println("KGrams + edit distance method. Time elapsed: " +
              (System.nanoTime() - startTimeFuzzyKGED) / 1000 + " usec. " +
              fuzzyKGEDResults.size + " results were found.")

            val startTimeFuzzyKG = System.nanoTime()
            val fuzzyKGResults = QueryProcessor.evaluateFuzzyQueryKGramOnly(inputString, index, kGramIndex)
            println("KGrams only method. Time elapsed: " +
              (System.nanoTime() - startTimeFuzzyKG) / 1000 + " usec. " +
              fuzzyKGResults.size + " results were found.")

            val startTimeFuzzyED = System.nanoTime()
            val fuzzyEDResults = QueryProcessor.evaluateFuzzyQueryEditDistanceOnly(inputString, index)
            println("Edit distance only method. Time elapsed: " +
              (System.nanoTime() - startTimeFuzzyED) / 1000 + " usec. " +
              fuzzyEDResults.size + " results were found.")
          }
        }
        prompt(scanner)
    }
  }
  prompt(scanner)
//  println(index
//    .map(pList => pList._2.foldLeft(0)((accumulator, posting) => posting._1 + accumulator)).sum)
//  index foreach println

//  println()
//  println("KGram index: ")
//  kGramIndex foreach println

//  println()
//  println("Permutation index: ")
//  permutationIndex foreach println

//  println()
//  val queryKGram = "board"
//  println("\"%s\" query using kgrams: ".format(queryKGram))
//  val resultKGram = QueryProcessor.evaluateWildcardQueryKGram(queryKGram, index, kGramIndex)
//  resultKGram foreach println

//  println()
//  val queryPermutation = "f*m d*y"
//  println("\"%s\" query using permutations: ".format(queryPermutation))
//  val resultPermutations = QueryProcessor.evaluateWildcardQueryPermutation(queryPermutation, index, permutationIndex)
//  resultPermutations foreach println
//
//  println()
//  val queryFuzzyKGED = "fiur indolacetic"
//  println("\"%s\" fuzzy query using kgram + edit distance: ".format(queryFuzzyKGED))
//  val resultFuzzyKGED = QueryProcessor.evaluateFuzzyQueryKGramEditDistance(queryFuzzyKGED, index, kGramIndex)
//  resultFuzzyKGED foreach println
//
//  println()
//  val queryFuzzyKG = "fiur indolacetic"
//  println("\"%s\" fuzzy query using kgram only: ".format(queryFuzzyKG))
//  val resultFuzzyKG = QueryProcessor.evaluateFuzzyQueryKGramOnly(queryFuzzyKG, index, kGramIndex)
//  resultFuzzyKG foreach println
//
//  println()
//  val queryFuzzyED = "fiur indolacetic"
//  println("\"%s\" fuzzy query using edit distance only: ".format(queryFuzzyED))
//  val resultFuzzyED = QueryProcessor.evaluateFuzzyQueryEditDistanceOnly(queryFuzzyED, index)
//  resultFuzzyED foreach println
}
