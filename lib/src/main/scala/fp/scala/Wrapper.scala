package fp.scala

class Wrapper[A] private (elem: A) {

  def map[B](f: A => B): Wrapper[B] = {
    Wrapper(f(elem))
  }

  def flatMap[B](f: A => Wrapper[B]): Wrapper[B] = {
    f(elem)
  }

  def elem(): A = elem
}

object Wrapper {
  def apply[A](elem: A): Wrapper[A] = new Wrapper(elem)
}
