package ru.otus

import ru.otus.module1.list.List.{incList, shoutString}
import ru.otus.module1.opt.Some
import ru.otus.module1.{hof, list, opt, type_system}

object Main {

  def main(args: Array[String]): Unit = {

    val q: opt.Option[Int] = Some(12)

    val q2: opt.Option[Int] = q
      .printIfAny
      .map(i => i + 10)
      .printIfAny
      .zip(q)
      .printIfAny
      .filter(pair => pair._1 > pair._2)
      .printIfAny
      .flatMap(pair => Some(pair._1 + pair._2))

    println(q2)

    ////

    val l1: list.List[Int] = list.List.apply(1, 2, 3)
    val l2: list.List[Int] = list.::(4, l1)
    val l4: list.List[String] = list.List("4", "5", "6")
    val l3q: list.List[Int] = l1 ::: l2

    println("small " + l1)
    println("concat " + l3q)
    l1 match {
      case list.Nil => println("matchintg:  List is empty")
      case list.::(h, t) => println(s"matchintg:  Head: $h, Tail: $t")
    }
    println("append " + l2)
    val l3 = list.List.reverse(l2)
    println("reversed " + l3)
    println("incList " + incList(l3))
    println("shoutString " + shoutString(l4))
    println("map " + list.List.map(l3)(x => x * 2))
    println("filter " + list.List.filter(l3)(x => x % 2 > 0))

  }
}
