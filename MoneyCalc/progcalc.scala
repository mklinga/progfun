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

  def howLongToReachInterestRate (desiredPerMonth: Double, savingsPerMonth: Double, money: Double, months: Int = 0): Int = {
    val interest = (money * interestPercentagePerMonth / 100)
    if (interest > desiredPerMonth) months
    else howLongToReachInterestRate(
      desiredPerMonth,
      savingsPerMonth,
      addMonthlyInterest(money + savingsPerMonth),
      months + 1
    )
  }

  val oneYearSavings = calculate(12, 500, 0)
  val fiveYearSavings = calculate(60, 500, 0)
  val tenYearSavings = calculate(120, 500, 0)

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
  val toReach1000 = howLongToReachInterestRate(1000, 500, 0, 0) / 12
  val toReach2000 = howLongToReachInterestRate(2000, 500, 0, 0) / 12
  println("To reach salary of 1000 euros per month on the interests => " + toReach1000 + " years")
  println("To reach salary of 2000 euros per month on the interests => " + toReach2000 + " years")
}
