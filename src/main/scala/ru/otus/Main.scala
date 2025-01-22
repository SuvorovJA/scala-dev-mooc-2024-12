package ru.otus

import ru.otus.module1.opt.Some
import ru.otus.module1.{hof, opt, type_system}

object Main {

  def main(args: Array[String]): Unit = {

    val q: opt.Option[Int] = Some(12)

    val q2: opt.Option[Int] = q
      .printIfAny
      .map(i => i + 10)
      .printIfAny
      .zip(q)
      .printIfAny
      .filter(pair => pair._1 > pair._2 )
      .printIfAny
      .flatMap(pair => Some(pair._1 + pair._2))

    println(q2)

  }
}
