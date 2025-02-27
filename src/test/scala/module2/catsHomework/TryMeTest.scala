package module2.catsHomework

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module2.catsHomework.tryME

class TryMeTest extends AnyFlatSpec with Matchers {
  import scala.util.{Failure, Success}

  "flatMap" should "apply to value inside Try" in {
    val result = tryME.flatMap(Success(1))(_ => Success(2))
    result shouldBe Success(2)
  }

  "pure" should "up value in Success" in {
    val result = tryME.pure(1)
    result shouldBe Success(1)
  }

  "raiseError" should "up error in Failure" in {
    val result = tryME.raiseError(new Exception("ошибка"))
    result shouldBe a[Failure[_]]
  }

  "handleErrorWith" should "recover error with another Try" in {
    val result = tryME.handleErrorWith(Failure(new Exception("ошибка")))(_ => Success(1))
    result shouldBe Success(1)
  }

  "handleError" should "recover error with another value" in {
    val result = tryME.handleError(Failure(new Exception("ошибка")))(_ => 1)
    result shouldBe Success(1)
  }

  "ensure" should "filter value inside Try" in {
    val result = tryME.ensure(Success(1))(new Exception("ошибка"))(_ > 2)
    result shouldBe a[Failure[_]]
  }
}

