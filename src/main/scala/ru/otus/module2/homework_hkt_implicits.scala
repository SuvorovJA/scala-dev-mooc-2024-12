package ru.otus.module2

import scala.util.Try

object homework_hkt_implicits {

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]

    def flatMap[B](f: A => F[B]): F[B]
  }

  def tupleF[F[_], A, B](fa: F[A], fb: F[B])
                        (implicit
                         ba: F[A] => Bindable[F, A],
                         bb: F[B] => Bindable[F, B]): F[(A, B)] = {
    fa.flatMap(a => fb.map((a, _)))
  }

  object Bindable {

    implicit class ListBindable[A](list: List[A]) extends Bindable[List, A] {
      override def map[B](f: A => B): List[B] = list.map(f)

      override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
    }

    implicit class SetBindable[A](set: Set[A]) extends Bindable[Set, A] {
      override def map[B](f: A => B): Set[B] = set.map(f)

      override def flatMap[B](f: A => Set[B]): Set[B] = set.flatMap(f)
    }

    implicit class OptionBindable[A](opt: Option[A]) extends Bindable[Option, A] {
      override def map[B](f: A => B): Option[B] = opt.map(f)

      override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
    }

    implicit class TryBindable[A](tr: Try[A]) extends Bindable[Try, A] {
      override def map[B](f: A => B): Try[B] = tr.map(f)

      override def flatMap[B](f: A => Try[B]): Try[B] = tr.flatMap(f)
    }

    // Either (╯°□°）╯︵ ┻━┻
    // todo

  }

}