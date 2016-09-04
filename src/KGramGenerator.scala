/**
  * Created by fonturacetamum on 04/09/16.
  */
object KGramGenerator {
  def generate(query: String, k: Int): List[String] = {
    query.split(" ")
      .map(word => (1 to word.length)
        .toList
        .map(index =>
          ("$" + word + "$").takeRight(word.length - index + 3).take(k)))
      .reduce(_ ::: _)
  }
}
