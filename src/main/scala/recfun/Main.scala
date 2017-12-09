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

    // if col is the beginning or end of the row, its value is 1
    if (c == 0 || c == r) {
      1
    // add values of index and one index prior fron row above
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    // filter out all the extra characters
    val p = chars.filter(c => c.toString == ")" || c.toString == "(")

    if (p.length <= 1) {
      p.isEmpty
    } else {
      if (p.startsWith("(")) {
        // drop open-close pairs from the beginning of the string
        if (p.tail.startsWith(")")) {
          balance(p.tail.tail)
          // otherwise rotate the characters
        } else {
          balance(List(p.head) ::: p.tail.tail ::: List(p.tail.head))
        }
        // rotate characters
      } else {
        balance(p.tail ::: List(p.head))
      }
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = 0
}
