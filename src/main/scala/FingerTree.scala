
sealed trait FingerTree[+A] {
  def +:[B >: A](x: B): FingerTree[B] = {
    this match
      case Empty => Single(x)
      case Single(y) => Deep(Digit1(x), Empty, Digit1(y))
      case Deep(Digit4(x1, x2, x3, x4), inner, d2) => Deep(Digit2(x, x1), Node3(x2, x3, x4) +: inner, d2)
      case Deep(Digit3(x1, x2, x3), inner, d2) => Deep(Digit4(x, x1, x2, x3), inner, d2)
      case Deep(Digit2(x1, x2), inner, d2) => Deep(Digit3(x, x1, x2), inner, d2)
      case Deep(Digit1(y), inner, d2) => Deep(Digit2(x, y), inner, d2)
  }
}

case object Empty extends FingerTree[Nothing]
case class Single[+A](x: A) extends FingerTree[A]
case class Deep[+A](x:Digit[A], mid: FingerTree[Node[A]], y: Digit[A])


sealed trait Node[+A] {}

case class Node2[+A](_1:A, _2: A) extends Node[A]
case class Node3[+A](_1:A,_2:A, _3:A) extends Node[A]

sealed trait Digit[+A]{
  def +:[B >: A](x: B): Option[Digit[B]] = {
    this match
      case Digit1(y) => Some(Digit2(x, y))
      case Digit2(y1, y2) => Some(Digit3(x, y1, y2))
      case Digit3(y1, y2, y3) => Some(Digit4(x, y1, y2, y3))
      case Digit4(y1, y2, y3, y4) => None
  }
}
case class Digit1[+A](_1: A) extends Digit[A]
case class Digit2[+A](_1:A, _2: A) extends Digit[A]
case class Digit3[+A](_1:A, _2: A, _3:A) extends Digit[A]
case class Digit4[+A](_1:A, _2:A, _3:A, _4: A) extends Digit[A]
