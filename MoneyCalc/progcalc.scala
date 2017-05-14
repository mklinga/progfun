object Progcalc extends App {
  val interestPercentagePerYear: Double = 7.0
  val interestPercentagePerMonth: Double = interestPercentagePerYear / 12.0

  def addMonthlyInterest (sum: Double): Double =
    sum + (sum * interestPercentagePerMonth / 100)


  def calculate(months: Int, savingsPerMonth: Double, money: Double): Double =
    if (months == 0) money
    else calculate(
      months - 1,
      savingsPerMonth,
      addMonthlyInterest(money + savingsPerMonth)
    )

  def howLongToReachInterestRate (desiredPerMonth: Double, savingsPerMonth: Double, money: Double, months: Int = 0): Double = {
    val interest: Double = (money * interestPercentagePerMonth / 100)
    if (interest > desiredPerMonth) months
    else howLongToReachInterestRate(
      desiredPerMonth,
      savingsPerMonth,
      addMonthlyInterest(money + savingsPerMonth),
      months + 1
    )
  }

  val oneYearSavings = calculate(12, 600, 0)
  val fiveYearSavings = calculate(60, 600, 0)
  val tenYearSavings = calculate(120, 600, 0)

  println()
  println("After one year = " + oneYearSavings)
  println("Monthly interest net = " + oneYearSavings * interestPercentagePerMonth / 100)
  println("Yearly interest net = " + oneYearSavings * interestPercentagePerYear / 100)
  println()
  println("After five years = " + fiveYearSavings)
  println("Monthly interest net = " + fiveYearSavings * interestPercentagePerMonth / 100)
  println("Yearly interest net = " + fiveYearSavings * interestPercentagePerYear / 100)
  println()
  println("After ten years = " + tenYearSavings)
  println("Monthly interest net = " + tenYearSavings * interestPercentagePerMonth / 100)
  println("Yearly interest net = " + tenYearSavings * interestPercentagePerYear / 100)

  println()
  val toReach500: Double = howLongToReachInterestRate(500, 600, 0, 0)
  val toReach1000: Double = howLongToReachInterestRate(1000, 600, 0, 0)

  def prettyPrint(target: Int, toReach: Double) = {
    val years: Int = Math.floor(toReach / 12).toInt
    val monthsExtra: Int = Math.round(toReach % 12).toInt
    println("To reach salary of " + target + " euros per month on the interests => " +
      years + " years, " + monthsExtra + " month(s)")

    println("In that time, you'd have paid " + toReach * 600 + " euros on rent")
  }

  prettyPrint(500, toReach500)
  prettyPrint(1000, toReach1000)

}
