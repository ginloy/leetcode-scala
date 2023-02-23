

sealed trait MyList[+T] {
  def ::[B >: T](y: B): MyList[B] = Cons(y, this)

  def head: Option[T] = this match {
    case Empty => Option.empty
    case Cons(x, _) => Option(x)
  }

  def tail: MyList[T] = this match {
    case Empty => this
    case Cons(_, xs) => xs
  }

  def +[B >: T](y: B): MyList[B] = this match {
    case Empty => Cons(y, this)
    case Cons(x, xs) => Cons(x, xs + y)
  }

  def ++[B >: T](lst: MyList[B]): MyList[B] = this match {
    case Empty => lst
    case Cons(x, xs) => Cons(x, xs ++ lst)
  }

  def reverse: MyList[T] = this match {
    case Empty => this
    case Cons(x, xs) => xs.reverse + x
  }

  def map[U](f: T => U): MyList[U] = this match {
    case Empty => Empty
    case Cons(x, xs) => Cons(f(x), xs.map(f))
  }

  def flatMap[U](f: T => MyList[U]): MyList[U] = this match {
    case Empty => Empty
    case Cons(x, xs) => f(x) ++ xs.flatMap(f)
  }

  def filter(f: T => Boolean): MyList[T] = this match {
    case Empty => this
    case Cons(x, xs) => if (f(x)) Cons(x, xs.filter(f)) else xs.filter(f)
  }

  override def toString: String = {
    def helper(lst : MyList[T]): String = {
      lst match {
        case Empty => ""
        case Cons(x, Empty) => x.toString
        case Cons(x, xs) => x.toString + ", " + helper(xs)
      }
    }
    s"[${helper(this)}]"
  }
}

case object Empty extends MyList[Nothing]

case class Cons[T](x: T, xs: MyList[T]) extends MyList[T]


object MyList {
  def apply[T](args: T*): MyList[T] = {
    if (args.isEmpty) Empty
    else Cons(args.head, apply(args.tail: _*))
  }

  def empty: MyList[Nothing] = Empty
}
