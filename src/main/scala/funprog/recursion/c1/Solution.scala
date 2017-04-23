package funprog.recursion.c1

/**
  * Computing the GCD
  * https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---gcd
  */
object Solution {

  def gcd(x: Int, y: Int): Int =
  {
    if (x > y) gcd(x-y, y)
    else if (x < y) gcd(y-x,x)
    else x
  }

  /**This part handles the input/output. Do not change or modify it **/
  def acceptInputAndComputeGCD(pair:List[Int]) = {
    println(gcd(pair.head,pair.reverse.head))
  }

  def main(args: Array[String]) {
    /** The part relates to the input/output. Do not change or modify it **/
    acceptInputAndComputeGCD(readLine().trim().split(" ").map(x=>x.toInt).toList)
  }
}

