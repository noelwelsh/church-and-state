package church

sealed trait Observable[A] {
  import Observable._

  def next(receiver: Receiver[A]): Unit

  def foldLeft[B](zero: B)(f: (A, B) => B): B = {
    // This is not fully CPSed as we'd blow the stack due to lack of true tail calls in Scala
    // We could trampoline but life is short.
    var result: B = zero
    val receiver = new StatefulReceiver[A]()

    next(receiver)
    while(!receiver.isEmpty) {
      result = f(receiver.get, result)
      next(receiver)
    }
    result
  }
}
object Observable {
  def fromStream[A](stream: Stream[A]): Observable[A] =
    stream match {
      case Stream.Zip(l, r) => Zip(fromStream(l), fromStream(r))
      case Stream.Map(s, f) => Map(fromStream(s), f)
      case Stream.FromIterator(s) => FromIterator(s)
    }

  def fromIterator[A](source: Iterator[A]): Observable[A] =
    FromIterator(source)

  def always[A](element: A): Observable[A] =
    FromIterator(Iterator.continually(element))

  def apply[A](elements: A*): Observable[A] =
    FromIterator(Iterator(elements: _*))

  // Observable algebraic data type
  final case class Zip[A,B](left: Observable[A], right: Observable[B]) extends Observable[(A,B)] {
    def next(downstream: Receiver[(A,B)]): Unit = {
      val leftReceiver = new Receiver[A] {
        def some(a: A): Unit = {
          val rightReceiver = new Receiver[B] {
            def some(b: B): Unit =
              downstream.some((a,b))

            def none: Unit =
              downstream.none
          }
          right.next(rightReceiver)
        }

        def none: Unit =
          downstream.none
      }
      left.next(leftReceiver)
    }
  }
  final case class Map[A,B](source: Observable[A], f: A => B) extends Observable[B] {

    def next(downstream: Receiver[B]): Unit = {
      val receiver = new Receiver[A] {
        def some(a: A): Unit =
          downstream.some(f(a))

        def none: Unit =
          downstream.none
      }
      source.next(receiver)
    }

  }
  final case class FromIterator[A](source: Iterator[A]) extends Observable[A] {
    def next(downstream: Receiver[A]): Unit =
      if(source.hasNext) downstream.some(source.next) else downstream.none
  }
}
