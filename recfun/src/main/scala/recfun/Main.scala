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
      if(c == 0 || c == r)
        1
      else
        pascal(c,r-1) + pascal(c-1,r-1)

    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        def b(l:List[Char],count:Int):Boolean = {
          if(l.isEmpty) {
             count == 0
          }
          else {
            if(count<0)
              false
            else
            l.head match {
                case '(' => b(l.tail, count + 1)
                case  ')' => b(l.tail,count - 1)
                case _ => b(l.tail,count)
            }
          }
        }
        b(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        def cc(m:Int,index:Int): Int ={
          if(m == 0)
            1
          else if ((m<0) || ((m > 0 ) && (index == coins.length)))
            0
          else
            cc(m-coins(index), index) +
            cc(m,index+1)
        }
      cc(money,0)
    }
  }
