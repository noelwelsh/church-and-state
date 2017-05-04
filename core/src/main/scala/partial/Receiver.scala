package partial

final class Receiver[A] {
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
