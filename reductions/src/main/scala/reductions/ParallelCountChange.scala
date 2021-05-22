package reductions

import org.scalameter._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
    } catch {
      case e: NotImplementedError =>
        println("Not implemented.")
    }

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange extends ParallelCountChangeInterface {

  /**
   * Returns the number of ways change can be made from the specified list of
   * coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }

  type Threshold = (Int, List[Int]) => Boolean

  /**
   * In parallel, counts the number of ways change can be made from the
   * specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (threshold(money, coins) || money <= 0 || coins.isEmpty)
      countChange(money, coins)
    else {
      val result = parallel(parCountChange(money - coins.head, coins, threshold), parCountChange(money, coins.tail, threshold))
      result._1 + result._2
    }
  }

  /**
   * Threshold heuristic based on the starting money.
   */
  def moneyThreshold(startingMoney: Int): Threshold = (amount, _) => amount <= (2 * startingMoney) / 3

  /**
   * Threshold heuristic based on the total number of initial coins.
   */
  def totalCoinsThreshold(totalCoins: Int): Threshold = (_, coins) => coins.size <= (2 * totalCoins) / 3

  /**
   * Threshold heuristic based on the starting money and the initial list of coins.
   */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (money, coins) => money * coins.size <= (startingMoney * allCoins.size) / 2
  }
}
