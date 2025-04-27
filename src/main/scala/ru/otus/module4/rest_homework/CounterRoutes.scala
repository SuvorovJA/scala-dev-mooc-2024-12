package ru.otus.module4.rest_homework

import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

class CounterRoutes[F[_] : Concurrent : Temporal] extends Http4sDsl[F] {

  // 1. Эндпоинт /counter
  def counterRoutes(counterRef: Ref[F, Int]): HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "counter" =>
      for {
        count <- counterRef.modify(c => (c + 1, c + 1))
        response <- Ok(Json.obj("counter" -> count.asJson))
      } yield response
  }

}