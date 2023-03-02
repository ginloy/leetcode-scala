import scala.annotation.targetName

sealed trait ImList[+T] {
  @targetName("prepend")
  def ::[B >: T](y: B): ImList[B] = Cons(y, this)

  def head: Option[T] = this match {
    case Empty => Option.empty
    case x :: _ => Option(x)
  }

  def tail: ImList[T] = this match {
    case Empty => this
    case _ :: xs => xs
  }

  @targetName("append")
  def +[B >: T](y: B): ImList[B] = this match {
    case Empty => y :: this
    case x :: xs => x :: xs + y
  }

  @targetName("concat")
  def ++[B >: T](lst: ImList[B]): ImList[B] = this match {
    case Empty => lst
    case x :: xs => x :: xs ++ lst
  }

  def reverse: ImList[T] = this match {
    case Empty => this
    case x :: xs => xs.reverse + x
  }

  def map[U](f: T => U): ImList[U] = this match {
    case Empty => Empty
    case x :: xs => f(x) :: xs.map(f)
  }

  def flatMap[U](f: T => ImList[U]): ImList[U] = this match {
    case Empty => Empty
    case x :: xs => f(x) ++ xs.flatMap(f)
  }

  def filter(f: T => Boolean): ImList[T] = this match {
    case Empty => this
    case x :: xs => if (f(x)) Cons(x, xs.filter(f)) else xs.filter(f)
  }

  override def toString: String = {
    def helper(lst : ImList[T]): String = {
      lst match {
        case Empty => ""
        case x :: Empty => x.toString
        case x :: xs => x.toString + ", " + helper(xs)
      }
    }
    s"[${helper(this)}]"
  }
}

case object Empty extends ImList[Nothing]

private case class Cons[T](x: T, xs: ImList[T]) extends ImList[T]

@targetName("Conned")
object :: {
  def unapply[A](lst : ImList[A]): Option[(A, ImList[A])] = {
    lst match {
      case Empty => None
      case Cons(x, xs) => Some((x, xs))
    }
  }
}

object ImList {
  def apply[T](args: T*): ImList[T] = {
    if (args.isEmpty) Empty
    else Cons(args.head, apply(args.tail: _*))
  }

  def empty: ImList[Nothing] = Empty
}
