package church

trait Receiver[A] {
  def some(a: A): Unit
  def none: Unit
}
final class StatefulReceiver[A]() extends Receiver[A] {
  var isEmpty: Boolean = true
  var get: A = _

  def some(a: A): Unit = {
    isEmpty = false
    get = a
  }

  def none: Unit = {
    isEmpty = true
  }
}
