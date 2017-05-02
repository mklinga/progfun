import scala.annotation.tailrec

object Factorial extends App {
  @tailrec
  def factorial (steps: Int, value: Int = 1): Int = {
    if (steps == 0) value else factorial(steps - 1, steps * value)
  }

  println(factorial(5))
}