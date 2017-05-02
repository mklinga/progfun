object Sqrt extends App {
  def abs(x:Double) = if (x < 0) -x else x
  def min(a: Double, b: Double) = if (a < b) a else b
  def max(a: Double, b: Double) = if (a > b) a else b

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) = {
    val epsilon = 0.0001
    (abs(guess * guess - x) / x < epsilon)
  }

  def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

  println("sqrt(1e-20)", sqrt(1e-20))
  println("sqrt(1e50)", sqrt(1e50))
}