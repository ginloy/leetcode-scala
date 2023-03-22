sealed trait FingerTree[+A] {
  import FingerTree.concatWithMiddle
  def +:[B >: A](x: B): FingerTree[B] = {
    this match {
      case Empty       => Single(x)
      case Single(y) => Deep(Digit1(x), Empty, Digit1(y))
      case Deep(left, mid, right) => left.insert(x) match {
        case Right(newLeft) => Deep(newLeft, mid, right)
        case Left((newLeft, node)) => Deep(newLeft, node +: mid, right)
      }
    }
  }

  def :+[B >: A](x: B): FingerTree[B] = {
    this match {
      case Empty       => Single(x)
      case Single(y) => Deep(Digit1(y), Empty, Digit1(x))
      case Deep(left, mid, right) => right.append(x) match {
        case Right(newRight) => Deep(left, mid, newRight)
        case Left((node, newRight)) => Deep(left, mid :+ node, newRight)
      }
    }
  }

  def ++[B >: A](other: FingerTree[B]): FingerTree[B] =
    concatWithMiddle(this, List(), other)

  def viewl: Option[(A, FingerTree[A])] = {
    this match {
      case Empty       => None
      case Single(x) => Some((x, Empty))
      case Deep(Digit1(x), xs, d2) => {
        xs.viewl match {
          case None => Some((x, Empty))
          case Some((Node2(y1, y2), ys)) =>
            Some((x, Deep(Digit2(y1, y2), ys, d2)))
          case Some((Node3(y1, y2, y3), ys)) =>
            Some((x, Deep(Digit3(y1, y2, y3), ys, d2)))
        }
      }
      case Deep(Digit4(x1, x2, x3, x4), xs, d) =>
        Some((x1, Deep(Digit3(x2, x3, x4), xs, d)))
      case Deep(Digit3(x, y, z), xs, d) => Some((x, Deep(Digit2(y, z), xs, d)))
      case Deep(Digit2(x, y), xs, d)    => Some((x, Deep(Digit1(y), xs, d)))
    }
  }

  def viewr: Option[(FingerTree[A], A)] = {
    this match {
      case Empty       => None
      case Single(x) => Some(Empty, x)
      case Deep(d1, xs, Digit1(x)) => {
        xs.viewr match {
          case None => Some((Empty, x))
          case Some((ys, Node2(y1, y2))) =>
            Some((Deep(d1, ys, Digit2(y1, y2)), x))
          case Some((ys, Node3(y1, y2, y3))) =>
            Some((Deep(d1, ys, Digit3(y1, y2, y3)), x))
        }
      }
      case Deep(d, xs, Digit4(x1, x2, x3, x4)) =>
        Some((Deep(d, xs, Digit3(x1, x2, x3)), x4))
      case Deep(d, xs, Digit3(x1, x2, x3)) =>
        Some((Deep(d, xs, Digit2(x1, x2)), x3))
      case Deep(d, xs, Digit2(x1, x2)) => Some((Deep(d, xs, Digit1(x1)), x2))
    }
  }

  def headOption: Option[A] = viewl match {
    case Some((x, _)) => Some(x)
    case _ => None
  }

  def lastOption: Option[A] = viewr match {
    case Some((_, x)) => Some(x)
    case _ => None
  }

  def tail: FingerTree[A] = viewl match {
    case Some((_, xs)) => xs
    case _ => Empty
  }

  def init: FingerTree[A] = viewr match {
    case Some((xs, _)) => xs
    case _ => Empty
  }

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

  def toList: List[A] = this match {
    case Empty => List.empty
    case Single(x) => List(x)
    case Deep(left, middle, right) => left.toList ++ middle.toList.flatMap(_.toList) ++ right.toList
  }

  def map[B](f: A => B): FingerTree[B] = this match {
    case Empty => Empty
    case Single(x) => Single(f(x))
    case Deep(left, middle, right) => Deep(left.map(f), middle.map(_.map(f)), right.map(f))
  }

  override def toString: String = {
    this.toList.toString
  }

}


object FingerTree {
  def apply[A](args: A*): FingerTree[A] = {
    args.foldLeft[FingerTree[A]](Empty)((v, x) => v :+ x)
  }
  private def nodes[A](lst: List[A]): Option[List[Node[A]]] = {
    lst match {
      case List(x, y, z) => Some(List(Node3(x, y, z)))
      case List(x, y) => Some(List(Node2(x, y)))
      case x :: y :: xs => nodes(xs).map(Node2(x, y) :: _)
      case _ => None
    }
  }
  private def concatWithMiddle[A](left : FingerTree[? <: A], lst: List[? <: A], right: FingerTree[? <: A]): FingerTree[A] = {
    (left, lst, right) match {
      case (Empty, List(), right) => right
      case (Empty, x :: xs, right) => x +: concatWithMiddle(Empty, xs, right)
      case (Single(x), xs, right) => x +: concatWithMiddle(Empty, xs, right)

      case (left, List(), Empty) => left
      case (left, xs, Empty) => concatWithMiddle(left, xs.init, Empty) :+ xs.last
      case (left, xs, Single(x)) => concatWithMiddle(left, xs, Empty) :+ x

      case (Deep(xl, xm, xr), mid, Deep(yl, ym, yr)) =>
        val mid2 = FingerTree.nodes(xr.toList ++ mid ++ yl.toList).getOrElse(List())
        Deep(xl, concatWithMiddle(xm, mid2, ym), yr)
    }
  }
   def fromList[A](lst: Seq[A]): FingerTree[A] = {
     lst.foldLeft[FingerTree[A]](Empty)((t, x) => t :+ x)
   }
}

private case object Empty extends FingerTree[Nothing]
private case class Single[+A](x: A) extends FingerTree[A]
case class Deep[+A](x: Digit[A], mid: FingerTree[Node[A]], y: Digit[A])
    extends FingerTree[A]

sealed trait Node[+A] {
  def toList: List[A] = this match {
    case Node2(a, b) => List(a, b)
    case Node3(a, b, c) => List(a, b, c)
  }

  def map[B](f: A=> B): Node[B] = this match {
    case Node2(a, b) => Node2(f(a), f(b))
    case Node3(a, b, c) => Node3(f(a), f(b), f(c))
  }
}
case class Node2[+A](_1: A, _2: A) extends Node[A]
case class Node3[+A](_1: A, _2: A, _3: A) extends Node[A]

sealed trait Digit[+A] {
  def toList: List[A] = this match {
    case Digit1(x) => List(x)
    case Digit2(x, y) => List(x, y)
    case Digit3(a, b, c) => List(a, b, c)
    case Digit4(a, b, c, d) => List(a, b, c, d)
  }

  def map[B](f: A => B): Digit[B] = this match {
    case Digit1(a) => Digit1(f(a))
    case Digit2(a, b) => Digit2(f(a), f(b))
    case Digit3(a, b, c) => Digit3(f(a), f(b), f(c))
    case Digit4(a, b, c, d) => Digit4(f(a), f(b), f(c), f(d))
  }

  def insert[B >: A](x : B): Either[(Digit[B], Node[B]), Digit[B]] = this match {
    case Digit1(a) => Right(Digit2(x, a))
    case Digit2(a, b) => Right(Digit3(x, a, b))
    case Digit3(a, b, c) => Right(Digit4(x, a, b, c))
    case Digit4(a, b, c, d) => Left((Digit2(x, a), Node3(b, c, d)))
  }

  def append[B >: A](x : B): Either[(Node[B], Digit[B]), Digit[B]] = this match {
    case Digit1(a) => Right(Digit2(a, x))
    case Digit2(a, b) => Right(Digit3(a, b, x))
    case Digit3(a, b, c) => Right(Digit4(a, b, c, x))
    case Digit4(a, b, c, d) => Left((Node3(a, b, c), Digit2(d, x)))
  }
}
case class Digit1[+A](_1: A) extends Digit[A]
case class Digit2[+A](_1: A, _2: A) extends Digit[A]
case class Digit3[+A](_1: A, _2: A, _3: A) extends Digit[A]
case class Digit4[+A](_1: A, _2: A, _3: A, _4: A) extends Digit[A]

object Main {
  def main(args: Array[String]): Unit = {

    val lst = FingerTree.fromList(0 to 1232)
    val lst2 = FingerTree.fromList(-200 to 1000)
    println((lst ++ lst2).map(_-500))
  }
}
