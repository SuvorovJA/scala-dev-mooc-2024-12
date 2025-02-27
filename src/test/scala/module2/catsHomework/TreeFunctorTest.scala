package module2.catsHomework

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.otus.module2.catsHomework.{Branch, Leaf, Tree, treeFunctor}

class TreeFunctorTest extends AnyFlatSpec with Matchers {
  "Tree Functor" should "map over Branch and Leaf" in {
    val tree: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val mappedTree: Tree[String] = treeFunctor.map(tree)(_.toString)

    mappedTree shouldBe Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3")))
  }
}
