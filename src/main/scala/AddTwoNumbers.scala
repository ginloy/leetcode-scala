object AddTwoNumbers extends App {
  def addTwoNumbers(l1: MyList[Int], l2: MyList[Int]): MyList[Int] = {
    def helper(l1: MyList[Int], l2: MyList[Int], carry: Int): MyList[Int] = {
      val res = (for {x <- l1.head; y <- l2.head} yield x + y + carry)
        .orElse(l1.head.map(x => x + carry))
        .orElse(l2.head.map(x => x + carry))
        .orElse(if (carry == 0) Option.empty else Option(carry))

      (for {
        total <- res
        digit = total % 10
        nextCarry = total / 10
      } yield digit :: helper(l1.tail, l2.tail, nextCarry))
        .getOrElse(MyList.empty)

    }

    helper(l1, l2, 0).reverse
  }

  sealed trait Person {
    def getName: String = this match {
      case Student(name, g) => name
      case Teacher(name, xs) => name
    }
  }

  case class Student(name: String, grade: Int) extends Person

  case class Teacher(name: String, students: MyList[Student]) extends Person

  val f: Person => String = x => x.getName
  val lst: MyList[Student] = MyList(Student("John", 123), Student("Tim", 235))

  println(lst.map(f))

  println(addTwoNumbers(MyList(9, 9, 9, 9, 9, 9, 9), MyList(9, 9, 9, 9)))
}
