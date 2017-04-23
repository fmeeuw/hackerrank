package funprog.recursion.c2

/**
  * Fibonacci Numbers
  * https://www.hackerrank.com/challenges/functional-programming-warmups-in-recursion---fibonacci-numbers?h_r=next-challenge&h_v=zen
  */
object Solution {

  def fibonacci(x:Int):Int = {
    if(x <= 1) 0
    else if(x == 2) 1
    else fibonacci(x-1) + fibonacci(x-2)
  }

  def main(args: Array[String]) {
    /** This will handle the input and output**/
    println(fibonacci(readInt()))

  }
}

