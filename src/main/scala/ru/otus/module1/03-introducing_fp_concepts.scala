package ru.otus.module1

import scala.annotation.tailrec
import scala.language.postfixOps


/**
 * referential transparency
 */


// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if (n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(x: Int, accum: Int): Int = {
      if (n <= 0) accum
      else loop(x - 1, x * accum)
    }

    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}


object hof {

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }



  // изменение поведения ф-ции


  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  lazy val isEven: Int => Boolean = not(isOdd)



  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)


  def sum(x: Int, y: Int): Int = x + y


  val p: Int => Int = partial(3, sum)
  p(2) // 5
  p(3) // 5


}


/**
 * Реализуем тип Option
 */


object opt {

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Variance
  // 1. Invariance
  // 2. Covariance
  // 3. Contrvariance

  trait Option[+T] {
    def isEmpty: Boolean = if (this.isInstanceOf[None.type]) true else false

    def get: T

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = if (isEmpty) None else f(this.get)

    /**
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[C >: T, B](that: Option[B]): Option[(C, B)] =
      if (isEmpty || that.isEmpty) None else Some((this.get, that.get))

    /**
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny: Option[T] =
      if (isEmpty) this else {
        println(this.get)
        this
      }

    /**
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(predicate: T => Boolean): Option[T] =
      if (isEmpty || predicate(this.get)) this else None
  }

  object Option {
    def apply[T](v: T): Option[T] = Some(v)
  }

  case class Some[T](v: T) extends Option[T] {
    def get: T = v
  }

  case object None extends Option[Nothing] {
    def get: Nothing = throw new NoSuchElementException("None.get always illegal")
  }

}

object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
   */


  trait List[+T] {
    def ::[TT >: T](elem: TT): List[TT] = ???
  }

  case class ::[T](head: T, tail: List[T]) extends List[T]

  case object Nil extends List[Nothing]

  object List {
    def apply[A](v: A*): List[A] = if (v.isEmpty) Nil
    else new::(v.head, apply(v.tail: _*))
  }

  val l1: List[Nothing] = List()
  val l2 = List(1, 2, 3)


  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_))
   */

  /**
   *
   * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
   */

  /**
   *
   * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
   */


  /**
   *
   * Реализовать метод filter для списка который будет фильтровать список по некому условию
   */

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */


  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */

}