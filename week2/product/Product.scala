object Product extends App {
  def general(f: Int => Int)(g: (Int, Int) => Int, i: Int)(a: Int, b: Int): Int = {
    if (a > b) i
    else g(f(a), general(f)(g, i)(a + 1, b))
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  println(product(x => x * x)(1, 3))
  println(general(x => x * x)((a: Int, b: Int) => a * b, 1)(1, 3))

  def fact(n: Int): Int = product(x => x)(1, n)
  def fact2(n: Int): Int = general(x => x)((a: Int, b: Int) => a * b, 1)(1, n)
  println(fact(5))
  println(fact2(5))
}