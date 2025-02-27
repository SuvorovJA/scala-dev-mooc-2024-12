package module2.catsHomework

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module2.catsHomework.eitherME

class EitherMeTest extends AnyFlatSpec with Matchers {

  "eitherME" should "Right value after pure()" in {
    val result = eitherME.pure(42)
    result shouldEqual Right(42)
  }

  it should "Left error after raiseError()" in {
    val result = eitherME.raiseError[Int]("ошибка")
    result shouldEqual Left("ошибка")
  }

  val f: Int => Right[Nothing, Int] = x => Right(x + 1)

  it should "apply to Right value in flatMap" in {
    val result = eitherME.flatMap(Right(42))(f)
    result shouldEqual Right(43)
  }

  it should "Left in flatMap when incoming Left" in {
    val result = eitherME.flatMap(Left("ошибка пришла"))(f)
    result shouldEqual Left("ошибка пришла")
  }

  it should "handle error handleErrorWith()" in {
    val result = eitherME.handleErrorWith(Left("ошибка пришла"))(e => Right(e.length))
    result shouldEqual Right(13)
  }

  it should "original value when no error in handleErrorWith()" in {
    val result = eitherME.handleErrorWith(Right(42))(e => Right(e.length))
    result shouldEqual Right(42)
  }

  it should "handle error in handleError()" in {
    val result = eitherME.handleError(Left("ошибка пришла"))(e => e.length)
    result shouldEqual Right(13)
  }

  it should "original value when no error in handleError()" in {
    val result = eitherME.handleError(Right(42))(e => e.length)
    result shouldEqual Right(42)
  }

  private val cond: Int => Boolean = i => i > 0

  it should "ensure condition" in {
    val result = eitherME.ensure(Right(42))("условие не прошло")(cond)
    result shouldEqual Right(42)
  }

  it should "Left when condition is not" in {
    val result = eitherME.ensure(Right(-1))("условие не прошло")(cond)
    result shouldEqual Left("условие не прошло")
  }

  it should "Left when Left incoming" in {
    val result = eitherME.ensure(Left("ошибка пришла"))("условие не прошло")(cond)
    result shouldEqual Left("ошибка пришла")
  }

}
