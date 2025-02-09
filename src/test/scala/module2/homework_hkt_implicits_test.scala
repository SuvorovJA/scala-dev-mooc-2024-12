package module2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module2.homework_hkt_implicits

import scala.util.{Failure, Success, Try}

class homework_hkt_implicits_test extends AnyFlatSpec with Matchers {
  {
    "tupleF" should "combine two lists" in {
      val list1 = List(42, 52, 43)
      val list2 = List("ZZ", "XX", "YY")
      val result = homework_hkt_implicits.tupleF(list1, list2)
      result should be(List(
        (42, "ZZ"), (42, "XX"), (42, "YY"),
        (52, "ZZ"), (52, "XX"), (52, "YY"),
        (43, "ZZ"), (43, "XX"), (43, "YY")
      ))
    }

    it should "combine two sets" in {
      val set1 = Set(42, 52, 43)
      val set2 = Set("ZZ", "XX", "YY")
      val result = homework_hkt_implicits.tupleF(set1, set2)
      result should be(Set(
        (42, "ZZ"), (42, "XX"), (42, "YY"),
        (52, "ZZ"), (52, "XX"), (52, "YY"),
        (43, "ZZ"), (43, "XX"), (43, "YY")
      ))
    }

    it should "combine two options" in {
      val opt1: Option[Int] = Some(42)
      val opt2: Option[String] = Some("ZZ")
      val result = homework_hkt_implicits.tupleF(opt1, opt2)
      result should be(Some((42, "ZZ")))
    }

    it should "combine two tries" in {
      val try1: Try[Int] = Success(42)
      val try2: Try[String] = Success("ZZ")
      val result = homework_hkt_implicits.tupleF(try1, try2)
      result should be(Success((42, "ZZ")))
    }

    it should "handle Option None" in {
      val opt1: Option[Nothing] = None
      val opt2: Option[String] = Some("ZZ")
      val result = homework_hkt_implicits.tupleF(opt1, opt2)
      result should be(None)
    }

    it should "handle Try Failure" in {
      val try1: Try[Int]  = Failure(new Exception("errorZZ"))
      val try2: Success[String] = Success("ZZ")
      val result = homework_hkt_implicits.tupleF(try1, try2)
      result shouldBe a[Failure[_]]
      result.failed.get.getMessage should be("errorZZ")
    }

  }
}
