package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def loop(nOfParentheses: Int, chars: Array[Char]): Boolean =
      if (chars.isEmpty) nOfParentheses == 0
      else if (nOfParentheses < 0) false
      else chars.head match {
        case '(' => loop(nOfParentheses + 1, chars.tail)
        case ')' => loop(nOfParentheses - 1, chars.tail)
        case _ => loop(nOfParentheses, chars.tail)
      }

    loop(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, leftParentheses: Int, rightParentheses: Int): (Int, Int) =
      if(idx == until) (leftParentheses, rightParentheses)
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, leftParentheses + 1, rightParentheses)
        case ')' =>
          if (leftParentheses > 0) traverse(idx + 1, until, leftParentheses - 1, rightParentheses)
          else traverse(idx + 1, until, leftParentheses, rightParentheses + 1)
        case _ => traverse(idx + 1, until, leftParentheses, rightParentheses)
      }


    def reduce(from: Int, until: Int): (Int, Int) = {
      val size = until - from
      if (size > threshold) {
        val mid = size / 2
        val ((a1, a2), (b1, b2)) = parallel(reduce(from, from + mid), reduce(from + mid, until))
        if (a1 > b2) {
          (a1 - b2 + b1) -> a2
        } else {
          b1 -> (b2 - a1 + a2)
        }
      }
      else {
        traverse(from, until, 0, 0)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
