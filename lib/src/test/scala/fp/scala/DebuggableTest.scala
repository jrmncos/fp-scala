package fp.scala

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DebuggableTest extends AnyFunSuite{

  def f(a: Int): Debuggable[Int] = {
    val result = a * 2
    val message = s"f: a ($a) * 2 = $result."
    Debuggable(result, List(message))
  }

  def g(a: Int): Debuggable[Int] = {
    val result = a * 3
    val message = s"g: a ($a) * 3 = $result."
    Debuggable(result, List(message))
  }

  def h(a: Int): Debuggable[Int] = {
    val result = a * 4
    val message = s"h: a ($a) * 4 = $result."
    Debuggable(result, List(message))
  }

  test("debuggable class work with for-yield expression") {
    val expected = f(100).flatMap { fResult =>
      g(fResult).flatMap{ gResult =>
        h(gResult).map{ hResult => hResult }
      }
    }

    val result = for {
      fResult <- f(100)
      gResult <- g(fResult)
      hResult <- h(gResult)
    } yield hResult

    assert(expected == result)
  }
}
