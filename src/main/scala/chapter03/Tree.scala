package chapter03

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match 
    case Leaf(_) => 1
    case Branch(l, r) => (l.depth + 1) max (r.depth + 1)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f1: A => B)(f2: (B, B) => B): B = this match
    case Leaf(value) => f1(value) 
    case Branch(l, r) => f2(l.fold(f1)(f2), r.fold(f1)(f2))

  def size2: Int = fold(_ => 1)(_ + _)

object Tree:
  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Tree.Leaf(i) => i
    case Tree.Branch(l, r) =>
      val lpos = l.firstPositive
      if lpos > 0 then lpos else r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Tree.Leaf(i) => i
    case Tree.Branch(l, r) =>
      l.maximum max r.maximum
