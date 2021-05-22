package reductions

import org.scalameter._

import scala.annotation.tailrec

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000
    val chars = new Array[Char](length)
    val threshold = 1000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /**
   * Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balance(chars: Array[Char], openCount: Int): Boolean = {
      if (chars.isEmpty) openCount == 0
      else if (openCount < 0) false
      else chars.head match {
        case '(' => balance(chars.tail, openCount + 1)
        case ')' => balance(chars.tail, openCount - 1)
        case _ => balance(chars.tail, openCount)
      }
    }

    balance(chars, 0)
  }

  /**
   * Returns `true` if the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, openCount: Int, closeCount: Int): (Int, Int) = {
      if (idx == until) (openCount, closeCount)
      else {
        chars(idx) match {
          case '(' => traverse(idx + 1, until, openCount + 1, closeCount)
          case ')' =>
            if (openCount > 0)
              traverse(idx + 1, until, openCount - 1, closeCount)
            else
              traverse(idx + 1, until, openCount, closeCount + 1)
          case _ => traverse(idx + 1, until, openCount, closeCount)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      }
      else {
        val mid = from + (until - from) / 2
        val ((lo, lc), (ro, rc)) = parallel(reduce(from, mid), reduce(mid, until))
        (lo + ro, lc + rc)
      }
    }

    if (chars.head == ')') return false

    val (left, right) = reduce(0, chars.length)
    left == right
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
