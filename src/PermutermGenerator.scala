/**
  * Created by fonturacetamum on 04/09/16.
  */
object PermutermGenerator {
  def generate(query: String): List[String] = {
    query.split(" ")
      .map(word => (1 to word.length + 1)
        .toList
        .map(index =>
          (word + "$").takeRight(word.length + 2 - index) + (word + "$").take(index - 1)
        ))
      .reduce(_ ::: _)
  }
}
