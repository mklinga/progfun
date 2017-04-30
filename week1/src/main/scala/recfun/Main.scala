package recfun

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
      if (c == 0 || c == r)  
        1
      else
        pascal(c - 1, r -1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def parenthesisSum(chars: List[Char], sum: Int = 0): Int = {

        if (chars.isEmpty) 
          return sum

        if (sum < 0)
          return -1

        if (chars.head == '(')
          parenthesisSum(chars.tail, sum + 1)
        else if (chars.head == ')')
          parenthesisSum(chars.tail, sum - 1)
        else
          parenthesisSum(chars.tail, sum)
      }

      parenthesisSum(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
