package chapter02

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
  def go(x: Int, y: Int, restSorted: Boolean): Boolean = 
    if (y >= as.length) restSorted
    else go(x + 1, y + 1, gt(as(y), as(x)) && restSorted)

  go(0, 1, true)
