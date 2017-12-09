package recfun

import scala.collection.mutable.ArrayBuffer

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
  // ())(
  // )(
  // ()
  //

  // (()())
  // (())(
  def balance(chars: List[Char]): Boolean = {
    // filter out all the extra characters
    val p = chars.filter(c => c.toString == ")" || c.toString == "(")

    if (p.length <= 1) {
      p.isEmpty
    } else {
      if (p.startsWith("(")) {
        if (p.tail.startsWith(")")) {
          // drop open-close pairs from the beginning of the string
          balance(p.tail.tail)
        } else {
          // another open paren
          !balance(p.tail)
        }
      // has to begin with open paren
      } else {
        false
      }
    }
  }

  /**
    * Exercise 3
    */

  // either you can make change with just that one coin or...
  // you can subtract from the total and make change with the remaining coins
//  countChange(300 ,List(500,5,50,100,20,200,10)) === 1022) => (300, 200, 100, 50, 30, 10, 5)
//  4,List(1,2)) === 3) => 4, (2, 1) should equal 3
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0 || coins.isEmpty) {
      0
    } else {
      var count: Int = 0

      def canMakeChangeWith(c1: Int, c2: Int): Boolean = (money - c1) % c2 == 0

      val srt = coins.sorted.reverse
      val h = srt.head
      val srt2 = coins.filter(_ <= h)

      srt2.foreach { c =>
        if (canMakeChangeWith(h, c)) {
          count += 1
        } // 4-2: +1

        count += countChange(money - c, srt2.tail) // 4-2: 2-2: +1 2-1: +1
//        count += countChange(money, srt2.tail)
      }

      count
    }
  }
}
