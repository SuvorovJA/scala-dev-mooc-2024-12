package ru.otus.module4.rest_homework

import cats.effect._
import com.comcast.ip4s._
import fs2.Stream
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.log4s.getLogger

import scala.concurrent.duration._
import scala.util.Random

object SlowStreamingApp extends IOApp {
  private val logger = getLogger

  def slowStreamService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "slow" / IntVar(chunk) / IntVar(total) / IntVar(time) =>
      if (chunk <= 0 || total <= 0 || time <= 0) {
        BadRequest("All parameters must be positive integers")
      } else {
        val randomChar = Random.nextPrintableChar()
        val chunksCount = (total.toDouble / chunk).ceil.toInt
        val content = randomChar.toString * chunk

        val responseStream = Stream.range(0, chunksCount)
          .evalTap(i => IO(logger.info(s"Generating chunk ${i + 1}")))
          .map(i => content.take(math.min(chunk, total - i * chunk)))
          .metered(time.seconds)
        //          .intersperse("\n")

        Ok(responseStream)
      }
  }

  def runClient(baseUri: Uri, chunk: Int, total: Int, time: Int): IO[Unit] = {
    EmberClientBuilder.default[IO].build.use { client =>
      val request = Request[IO](
        method = GET,
        uri = baseUri.withPath(Uri.Path.unsafeFromString(s"/slow/$chunk/$total/$time"))
      )

      // Создаем Ref для счетчика
      Ref.of[IO, Int](0).flatMap { counter =>
        // Обрабатываем запрос и ответ
        client.run(request).use { response =>
          // Стримим тело ответа
          response.body
            .chunks
            .evalTap { chunk => // Для каждого чанка
              // Обновляем счетчик и логируем
              counter.updateAndGet(_ + 1).flatMap { count =>
                IO(logger.info(s"Received chunk #$count, size: ${chunk.size} bytes")) >>
                  IO(logger.info(s"Content: ${chunk.toByteBuffer.array().map(_.toChar).mkString}"))
              }
            }
            .compile
            .drain
        } >> IO(logger.info(s"Total response size: $total bytes"))
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val server = EmberServerBuilder.default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(slowStreamService.orNotFound)
      .build

    val clientApp = runClient(uri"http://localhost:8080", 85, 333, 1)

    server.flatMap(_ => Resource.eval(clientApp)).use(_ => IO.pure(ExitCode.Success))
  }
}
// runClient(uri"http://localhost:8080", 85, 333, 1)
//14:20:30.306 [io-compute-3] INFO org.http4s.ember.server.EmberServerBuilderCompanionPlatform - Ember-Server service bound to address: [::]:8080
//14:20:31.580 [io-compute-16] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Generating chunk 1
//14:20:31.589 [io-compute-9] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Received chunk #1, size: 85 bytes
//14:20:31.589 [io-compute-9] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Content: CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
//14:20:32.577 [io-compute-21] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Generating chunk 2
//14:20:32.578 [io-compute-1] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Received chunk #2, size: 85 bytes
//14:20:32.578 [io-compute-1] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Content: CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
//14:20:33.580 [io-compute-16] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Generating chunk 3
//14:20:33.581 [io-compute-21] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Received chunk #3, size: 85 bytes
//14:20:33.581 [io-compute-21] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Content: CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
//14:20:34.584 [io-compute-15] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Generating chunk 4
//14:20:34.585 [io-compute-7] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Received chunk #4, size: 78 bytes
//14:20:34.585 [io-compute-7] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Content: CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
//14:20:35.587 [io-compute-21] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Total response size: 333 bytes
//
//Process finished with exit code 0

//runClient(uri"http://localhost:8080", 85, 333, -1)
//14:21:08.319 [io-compute-17] INFO org.http4s.ember.server.EmberServerBuilderCompanionPlatform - Ember-Server service bound to address: [::]:8080
//14:21:08.567 [io-compute-2] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Received chunk #1, size: 40 bytes
//14:21:08.568 [io-compute-2] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Content: All parameters must be positive integers
//14:21:08.571 [io-compute-2] INFO ru.otus.module4.rest_homework.SlowStreamingApp - Total response size: 333 bytes