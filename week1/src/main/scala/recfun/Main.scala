package recfun

object Main {
  def main(args: Array[String]) {
    countChange(4, List[Int](1, 2))
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
    def countChange(money: Int, coins: List[Int]): Int = {

      var matches: Int = 0
      if (money == 0 || coins.isEmpty) return 0

      def addNext(sum: Int, next: Int): Int = {
        sum + next
      }

      def addRest(sum: Int, rest: List[Int]): Unit = {
        if (rest.isEmpty) {
          return;
        }
        
        addUntilOver(sum, rest.head, rest.tail)
      }

      def addUntilOver(sum: Int, current: Int, rest: List[Int]): Unit = {
        if (sum < money) {
          addRest(sum, rest)
          return addUntilOver(addNext(sum, current), current, rest)
        }
        else if (sum == money) {
          matches += 1
        }
      }

      addUntilOver(0, coins.head, coins.tail)
      matches
    }
  }
