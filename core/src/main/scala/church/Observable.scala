package church

sealed trait Observable[A] {
  import Observable._

  def next(receiver: Receiver[A]): Unit

  def foldLeft[B](zero: B)(f: (A, B) => B): B = {
    var result: B = zero
    val receiver = new Receiver[A]()

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
    private val leftReceiver = new Receiver[A]()
    private val rightReceiver = new Receiver[B]()

    def next(receiver: Receiver[(A,B)]): Unit = {
      left.next(leftReceiver)
      if(leftReceiver.isEmpty) {
        receiver.none
      } else {
        right.next(rightReceiver)
        if(rightReceiver.isEmpty) {
          receiver.none
        } else {
          receiver.some((leftReceiver.get, rightReceiver.get))
        }
      }
    }
  }
  final case class Map[A,B](source: Observable[A], f: A => B) extends Observable[B] {
    private val upstream = new Receiver[A]()

    def next(receiver: Receiver[B]): Unit = {
      source.next(upstream)
      if(upstream.isEmpty) receiver.none else receiver.some(f(upstream.get))
    }

  }
  final case class FromIterator[A](source: Iterator[A]) extends Observable[A] {
    def next(receiver: Receiver[A]): Unit =
      if(source.hasNext) receiver.some(source.next) else receiver.none
  }
}
