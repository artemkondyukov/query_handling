/**
  * Created by fonturacetamum on 04/09/16.
  */
object Levenshtein {
  val insertion_cost = 1
  val alphabet = (1 to 26).toList.map(n => (n + 96).toChar)
  // By now it's just a mock, but substitution matrix should represent relative positions of keys on the keyboard
  val sub_matrix = alphabet.flatMap(char => alphabet.map(char_in =>
    (char -> char_in) -> (if (char == char_in) 0 else 1)))
    .toMap

  def table(string_1: String, string_2: String): List[List[Int]] = {
    def min(a: Int, b: Int, c: Int) = { Math.min(Math.min(a, b), c) }

    string_1.scanLeft((0 to string_2.length).toList) ((row, letter_1) =>
      (row zip row.tail zip string_2).scanLeft(row.head + 1) {
        // There are three possible ways:
        // go from the left, go from the top and go from the left-top diagonal
        case (left, ((diag, top), letter_2)) =>
          min(left + 1, top + 1, diag + sub_matrix((letter_1, letter_2)))
      }
    )
      .toList
  }

  def render(string_1: String, string_2: String): String = {
    table(string_1, string_2).map(row => row.map(_ + "\t").reduce(_ + _) + "\n").reduce(_ + _)
  }

  def distance(string_1: String, string_2: String): Int = {
    table(string_1, string_2).last.last
  }
}
