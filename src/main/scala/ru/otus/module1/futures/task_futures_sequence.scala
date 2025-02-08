package ru.otus.module1.futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
    //task"Реализуйте метод `fullSequence`" ()

    //// "full sequence" 15 runnable
    //// "process list of success and failures" 11 runnable
    //// "process list of failures" 6 runnable
    //    Future.traverse(futures) {
    //        _.map(Right(_)).recover { case throwable => Left(throwable) }
    //      }
    //      .map {
    //        _.partitionMap {
    //          case Left(t) => Right(t)
    //          case Right(a) => Left(a)
    //        }
    //      }

    //// "full sequence" 12 runnable
    //// "process list of success and failures" 8 runnable
    //// "process list of failures" 4 runnable
    futures.foldRight(Future.successful((List.empty[A], List.empty[Throwable]))) {
      case (future, acc) =>
        acc.flatMap {
          case (successes, failures) =>
            future.map { result => (result :: successes, failures) }
              .recover { case throwable => (successes, throwable :: failures) }
        }
    }
}