/**
  * Created by fonturacetamum on 04/09/16.
  */

object KGramGenerator {
  def generate(query: String, k: Int = 2): List[String] = {
    query.split(" ")
      .map(word => (1 to word.length + 1)
        .toList
        .map(index =>
          ("$" + word + "$").takeRight(word.length - index + 3).take(k)))
      .reduce(_ ::: _)
  }
}
