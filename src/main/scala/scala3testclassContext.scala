class GivenInt(using val usingParam: Int)(using val usingParam1: String){}

object scala3testclassContext {
  @main def scala3testclassContextEx(): Unit = {

    val b = GivenInt(using 42)(using "test")
    import b.given


    println(usingParam)
    println(usingParam1)

    println(summon[Int])
    println(summon[String])

  }


}