package recfun

import scala.annotation.tailrec

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
      def isFirstElement(c: Int, r: Int): Boolean = c == 0 || c == r
      if(isFirstElement(c, r)) 1
      else pascal(c - 1, r - 1) + pascal(c, r -1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      @tailrec
      def loop(nOfLeftParentheses: Int, nOfRightParentheses: Int, chars: List[Char]): Boolean =
        if(chars.isEmpty) nOfLeftParentheses == nOfRightParentheses
        else if(nOfLeftParentheses < nOfRightParentheses) false
        else if(chars.head == '(') loop(nOfLeftParentheses + 1, nOfRightParentheses, chars.tail)
        else if(chars.head == ')') loop(nOfLeftParentheses, nOfRightParentheses + 1, chars.tail)
        else loop(nOfLeftParentheses, nOfRightParentheses, chars.tail)

      loop(0, 0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
