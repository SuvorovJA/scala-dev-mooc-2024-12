package monad

object for_comprehension {

  def main(args: Array[String]): Unit = {
    val result: Wrap[Int] = NonEmptyWrap(10)

    println("for_comprehension:")
    val r1 = for {
      res <- result
    } yield res
    println(r1)

    val anotherResult: Wrap[Int] = NonEmptyWrap(100)

    val r2 =for {
      res <- result
      another <- anotherResult
    } yield res + another
    println(r2)

    val r3 = for {
      res <- result
      another <- anotherResult
      if res > 10
    } yield res + another
    println(r3)

    println("desugar:")
    val r1desugar: Wrap[Int] = result.map((res: Int) => res)
    println(r1desugar)
    val r2desugar: Wrap[Int] = result.flatMap(res => anotherResult.map(another => res.+(another)) )
    println(r2desugar)
    val r3desugar: Wrap[Int] = result.flatMap(res => anotherResult.withFilter(another => res.>(10)).map(another => res.+(another)))
    println(r3desugar)
  }

}