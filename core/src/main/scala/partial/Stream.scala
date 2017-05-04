package partial

/**
  * Partially church encoded / CPSed implementation
  */
sealed trait Stream[A] {
  import Stream._

  def zip[B](that: Stream[B]): Stream[(A,B)] =
    Zip(this, that)

  def map[B](f: A => B): Stream[B] =
    Map(this, f)

  def foldLeft[B](zero: B)(f: (A, B) => B): B = {
    val observable = Observable.fromStream(this)
    observable.foldLeft(zero)(f)
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
