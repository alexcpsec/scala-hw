package recfun
import common._

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
    if (c == 0) 1
    else {
      if (c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1) 
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val openPar = '('
    val closePar = ')'
    
    def balanceRec(chars: List[Char], acc: Int): Int = {
      if (chars.isEmpty || acc < 0) acc
      else {
	      val c = chars.head
	      if (c == openPar) balanceRec(chars.tail, acc + 1)
	      else {
	    	  if (c == closePar) balanceRec(chars.tail, acc - 1)
	    	  else balanceRec(chars.tail, acc)
	      }
      }
    }
    
    balanceRec(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else {
	    if (money == 0) 1
	    else {
		    countChange(money - coins.head, coins) + countChange(money, coins.tail)
	    }
    }
  }
}
