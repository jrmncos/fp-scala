package fp.scala

case class Debuggable[A](value: A, log: List[String]) {
  def map[B](f: A => B): Debuggable[B] = {
    Debuggable(f(value), log)
  }

  def flatMap[B](f: A => Debuggable[B]): Debuggable[B] ={
    val nextValue: Debuggable[B] = f(value)
    Debuggable(nextValue.value, log ::: nextValue.log)
  }
}
