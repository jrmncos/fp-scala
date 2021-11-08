package fp.scala

case class State[S, A](run: S => (S, A)){

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State{ previousState =>
      val (actualState, a) = run(previousState)
      val stateChangedToB = f(a)
      val (nextState, b) = stateChangedToB.run(actualState)
      (nextState, b)
    }
  }

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.wrap(f(a)))
  }
}

object State {
  //Wrap value into a State[S, A]
  def wrap[A, S](value: A): State[S, A] = State( state => (state, value))
}
