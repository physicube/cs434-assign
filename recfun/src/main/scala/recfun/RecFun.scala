package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
  {
    if (c == 0 || r == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
  {
    def balance_aux(chars: List[Char], depth:Int): Boolean =
    {
      chars match {
        case Nil => if (depth > 0) false else true
        case head :: tl => head match {
          case '(' => balance_aux(tl, depth + 1)
          case ')' => if (depth > 0) balance_aux(tl, depth - 1) else false
          case _ => balance_aux(tl, depth)
        }
      }
    }
    balance_aux(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
  {
    def countCoin(money: Int, coin_left: List[Int]): Int = 
    {
      coin_left match {
        case Nil => 0
        case head :: tl => countChange(money - head, coin_left) + countChange(money, tl)
      }
    }
    if (money < 0) 0 else {if (money == 0) 1 else countCoin(money, coins.sorted(Ordering.Int.reverse))}
  }
}
