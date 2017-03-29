package termination

/**
  * Implementation of reactive streams that takes the baseline implementation and adds a signal that indicates when a stream has terminated.
  */
sealed trait Stream[A] {
  import Stream._

  def zip[B](that: Stream[B]): Stream[(A,B)] =
    Zip(this, that)

  def map[B](f: A => B): Stream[B] =
    Map(this, f)

  def foldLeft[B](zero: B)(f: (A, B) => B): B = {
    // Use `Option` to indicate if the stream has terminated. `None` indicates no more values are available.
    def next[A](stream: Stream[A]): Option[A] =
      stream match {
        case FromIterator(source) =>
          if(source.hasNext) Some(source.next()) else None
        case Map(source, f) =>
          next(source).map(f)
        case Zip(left, right) =>
          for {
            l <- next(left)
            r <- next(right)
          } yield (l, r)
      }

    var result: B = zero
    var run: Boolean = true

    while(run) {
      next(this) match {
        case None => run = false
        case Some(a) =>
          result = f(a, result)
      }
    }
    result
  }
}
object Stream {
  def fromIterator[A](source: Iterator[A]): Stream[A] =
    FromIterator(source)

  def always[A](element: A): Stream[A] =
    FromIterator(Iterator.continually(element))

  def apply[A](elements: A*): Stream[A] =
    FromIterator(Iterator(elements: _*))

  // Stream algebraic data type
  final case class Zip[A,B](left: Stream[A], right: Stream[B]) extends Stream[(A,B)]
  final case class Map[A,B](source: Stream[A], f: A => B) extends Stream[B]
  final case class FromIterator[A](source: Iterator[A]) extends Stream[A]
}
