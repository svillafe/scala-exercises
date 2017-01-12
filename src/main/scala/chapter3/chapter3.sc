
sealed trait LinkedList[+A]
case object Nil extends LinkedList[Nothing]
case class Cons[+A](head : A, tail : LinkedList [A]) extends LinkedList[A]

object LinkedList {
  def sum(ints: LinkedList[Int]) : Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: LinkedList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](list: LinkedList[A]): LinkedList[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[A](list: LinkedList[A], n: Int): LinkedList[A] = {
    if(n < 1) list
    else list match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  //Ex 3.4
  def setHead[A](list: LinkedList[A], x: A) = list match {
    case Nil => Cons(x, Nil)
    case Cons(y, ys) => Cons(x, Cons(y, ys))
  }

  //Ex 3.5
  def dropWhile1[A](list: LinkedList[A], f: A => Boolean): LinkedList[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if(f(x)) dropWhile1(xs, f) else Cons(x,xs)
  }


  def dropWhile2[A](list: LinkedList[A], f: A => Boolean): LinkedList[A] = list match {
    case Cons(x, xs) if(f(x)) =>  dropWhile2(xs, f)
    case _ => list

  }

  //E.g. page 36
  //Note this definition only copies values until the first list is exhausted.
  //Therefore, its T(n) = length(l1) + 1 i.e. its time complexity is determined by the length of l1.
  def append[A](l1: LinkedList[A], l2: LinkedList[A]): LinkedList[A] = l1 match{
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

  //Ex. 3.6
  def init[A](l: LinkedList[A]): LinkedList[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if xs == Nil => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](l: LinkedList[A]): LinkedList[A] = l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  
  def apply[A](as: A*): LinkedList[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

val x = LinkedList[Int](1,2,3,4,5)

x match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + LinkedList.sum(t)
  case _ => 101
}

LinkedList.tail(x)
LinkedList.setHead(x, 8)
LinkedList.drop(x, 3)
LinkedList.dropWhile1(x, (x:Int) => x < 2)
LinkedList.dropWhile2(x, (x:Int) => x < 3)
LinkedList.init(x)
LinkedList.init2(x)



