package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c > (r + 1)) {
      throw new NoSuchElementException
    }

    // If col is the beginning or end of the row, its value is 1
    if (c == 0 || c == r) {
      1
      // Add values of index and one index prior fron row above
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    // Filter out all the extra characters
    val p = chars.filter(c => c.toString == ")" || c.toString == "(")

    val opens: Boolean = p.startsWith("(")
    val ends: Boolean = p.endsWith(")")
    val closes: Boolean = p.tail.startsWith(")")
    // drop first two characters () and check the rest of the string
    lazy val tailIsBalanced: Boolean = balance(p.tail.tail)

    // Starting with a close paren and ending with an open paren are invalid
    if (!opens || !ends) {
      false
    } else {
      // If the string starts with an open and close paren, the rest of the tail must be balanced.
      // Conversely, if the string starts with multiple open parens, it must also end with the same number of closing parens.
      (closes && tailIsBalanced) || (!closes && !tailIsBalanced)
    }
    // condensed: (opens && ends) && (closes && tailIsBalanced) || (!closes && !tailIsBalanced)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0 || coins.isEmpty) {
      0
    } else {
      var count: Int = 0

      def canMakeChangeWith(c1: Int, c2: Int): Boolean = (money - c1) % c2 == 0

      val srt = coins.filter(_ <= money).sorted.reverse
      val h = srt.headOption.getOrElse(0)

      srt.foreach { c =>
        if (canMakeChangeWith(h, c)) {
          count += 1
        }

        if (c > 1) {
          count += countChange(money - c, srt.tail)
        }
      }

      count
    }
  }
}