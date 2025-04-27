package ru.otus.module4.rest_homework

import cats.effect._
import cats.effect.implicits.effectResourceOps
import com.comcast.ip4s.IpLiteralSyntax
import io.circe.Json
import io.circe.generic.decoding.DerivedDecoder.deriveDecoder
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._

object MainCounter extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {

    val resources = for {

      counterRef <- Ref.of[IO, Int](0).toResource
      routes = new CounterRoutes[IO].counterRoutes(counterRef)

      server <- EmberServerBuilder
        .default[IO]
        .withHost(ipv4"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(routes.orNotFound)
        .build

      client <- EmberClientBuilder.default[IO].build

    } yield (server, client)

    resources.use { case (server, client) =>
      for {
        _ <- IO.println(s"Server started at ${server.address}")

        _ <- client.expect[Json](uri"http://localhost:8080/counter")
          .flatMap(resp => IO.println(s"Counter: $resp"))
        _ <- client.expect[Json](uri"http://localhost:8080/counter")
          .flatMap(resp => IO.println(s"Counter: $resp"))
        _ <- client.expect[Json](uri"http://localhost:8080/counter")
          .flatMap(resp => IO.println(s"Counter: $resp"))

      } yield ExitCode.Success
    }
  }

}

// 13:08:28.978 [io-compute-16] INFO org.http4s.ember.server.EmberServerBuilderCompanionPlatform - Ember-Server service bound to address: [::]:8080
//Server started at /[0:0:0:0:0:0:0:0]:8080
//Counter: {
//  "counter" : 1
//}
//Counter: {
//  "counter" : 2
//}
//Counter: {
//  "counter" : 3
//}
//
//Process finished with exit code 0