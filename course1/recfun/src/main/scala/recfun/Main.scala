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

  /*
   - 가장 큰돈 보다 남은 돈이 클 경우
    * 가장 큰 돈을 한번 더 빼본다.
    * 그 다음 큰돈을 한번 적용 해본다.
   - 가장 큰돈 보다 남은 돈이 적을 경우
    * 다음으로 큰돈으로 적용 해본다.
   */
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def subtractMoney(moneyRemain: Int, coins: List[Int]): Int = {
        if(moneyRemain == 0) 1
        else if(coins.isEmpty) 0
        else if(moneyRemain >= coins.head) subtractMoney(moneyRemain - coins.head, coins) + subtractMoney(moneyRemain, coins.tail)
        else subtractMoney(moneyRemain, coins.tail)
      }

      subtractMoney(money, coins.reverse)
    }
  }
