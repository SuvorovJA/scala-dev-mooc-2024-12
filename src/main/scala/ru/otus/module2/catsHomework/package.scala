package ru.otus.module2
import cats.Functor

import scala.util.{Failure, Success, Try}

package object catsHomework {

  /**
   * Простое бинарное дерево
   * @tparam A
   */
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  /**
   * Напишите instance Functor для объявленного выше бинарного дерева.
   * Проверьте, что код работает корректно для Branch и Leaf
   */

  lazy val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  /**
   * Monad абстракция для последовательной
   * комбинации вычислений в контексте F
   * @tparam F
   */
  trait Monad[F[_]]{
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](v: A): F[A]
  }


  /**
   * MonadError расширяет возможность Monad
   * кроме последовательного применения функций, позволяет обрабатывать ошибки
   * @tparam F
   * @tparam E
   */
  trait MonadError[F[_], E] extends Monad[F]{
    // Поднимаем ошибку в контекст `F`:
    def raiseError[A](e: E): F[A]

    // Обработка ошибки, потенциальное восстановление:
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    // Обработка ошибок, восстановление от них:
    def handleError[A](fa: F[A])(f: E => A): F[A]

    // Test an instance of `F`,
    // failing if the predicate is not satisfied:
    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }

  /**
   * Напишите instance MonadError для Try
   */

   lazy val tryME = new MonadError[Try, Throwable] {
     override def pure[A](v: A): Try[A] = Success(v)
     override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
     override def raiseError[A](e: Throwable): Try[A] = Failure(e)
     override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa.recoverWith { case e => f(e) }
     override def handleError[A](fa: Try[A])(f: Throwable => A): Try[A] = fa.recover { case e => f(e) }
     override def ensure[A](fa: Try[A])(e: Throwable)(f: A => Boolean): Try[A] = fa.filter(f)
   }

  /**
   * Напишите instance MonadError для Either,
   * где в качестве типа ошибки будет String
   */
   type Eit[A] = Either[String, A]
   val eitherME = new MonadError[Eit, String] {
     override def pure[A](v: A): Eit[A] = Right(v)

     override def flatMap[A, B](fa: Eit[A])(f: A => Eit[B]): Eit[B] = fa match {
       case Left(e) => Left(e)
       case Right(a) => f(a)
     }

     override def raiseError[A](e: String): Eit[A] = Left(e)

     override def handleErrorWith[A](fa: Eit[A])(f: String => Eit[A]): Eit[A] = fa match {
       case Left(e) => f(e)
       case Right(a) => Right(a)
     }

     override def handleError[A](fa: Eit[A])(f: String => A): Eit[A] = fa match {
       case Left(e) => Right(f(e))
       case Right(a) => Right(a)
     }

     override def ensure[A](fa: Eit[A])(e: String)(f: A => Boolean): Eit[A] = fa match {
       case Left(e) => Left(e)
       case Right(a) => if (f(a)) Right(a) else Left(e)
     }
   }
}
