package chapter02

def fib(n: Int): Int = 
  @annotation.tailrec
  def go(x: Int, y: Int, n: Int): Int =
    if (n <= 0) x
    else go(y, x+y, n-1)
  
  go(0, 1, n)
  