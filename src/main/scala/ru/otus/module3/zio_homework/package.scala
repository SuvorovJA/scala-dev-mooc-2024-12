package ru.otus.module3

import ru.otus.module3.zioConcurrency.printEffectRunningTime
import ru.otus.module3.zio_homework.config.{AppConfig, Configuration}
import zio.Clock.ClockLive
import zio.Console._
import zio.Random.{RandomLive, nextIntBetween}
import zio.{Clock, Console, ExitCode, Random, URLayer, ZIO, ZLayer, durationInt}

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу, которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  private val generateNumber: ZIO[Any, Nothing, Int] = nextIntBetween(1, 4)

  private val getUserGuess: ZIO[Any, IOException, Int] =
    readLine.flatMap { input =>
      ZIO.attempt(input.toInt).orElse {
        printLine("введите число а не символ: ") zipRight getUserGuess
      }
    }

  lazy val guessProgram: ZIO[Any, IOException, Unit] = {
    for {
      _ <- printLine("Введите число от 1 до 3, посмотрим угадали ли:")
      secret <- generateNumber
      guess <- getUserGuess
      _ <- if (guess == secret) printLine("угадали!")
      else printLine(s"Не угадали! было $secret.")
    } yield ()
  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  private def doWhile[R, E, A](effect: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] = {
    effect.flatMap { result: A =>
      if (condition(result)) {
        ZIO.succeed(result)
      } else {
        doWhile(effect)(condition)
      }
    }
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль.
   * Используйте эффект "Configuration.config" из пакета config
   */

  private val defaultConfig = AppConfig("localhost", "8080")

  def loadConfigOrDefault: ZIO[Any, IOException, AppConfig] =
    Configuration.config.catchAll { error =>
      for {
        _ <- printLine(s"Ошибка загрузки конфига: ${error.getMessage}")
        _ <- printLine(s"Используем дефолтный конфиг: $defaultConfig")
      } yield defaultConfig
    }

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */

  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  private lazy val eff: ZIO[Random with Clock, Nothing, Int] = ZIO.sleep(1.second) zipRight nextIntBetween(0, 11)

  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  private lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] = List.fill(10)(eff)

  /**
   * 4.3 Напишите программу, которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение.
   * Можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях.
   */

  private val sumProgram: ZIO[Random with Clock with Console, Nothing, Int] =
    printEffectRunningTime {
      ZIO.collectAll(effects).map(_.sum)
    }

  lazy val app: ZIO[Any, Throwable, ExitCode] = (for {
    sum <- sumProgram
    _ <- printLine(s"Sum of numbers: $sum")
  } yield ExitCode.success)
    .catchAll(e => printLine(s"Error: ${e.getMessage}").as(ExitCode.failure))
    .provide(ZLayer.succeed(ConsoleLive), ZLayer.succeed(RandomLive), ZLayer.succeed(ClockLive))

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  private val sumProgramParallel: ZIO[Random with Clock with Console, Nothing, Int] =
    printEffectRunningTime {
      ZIO.collectAllPar(effects).map(_.sum)
    }

  lazy val appSpeedUp: ZIO[Any, IOException, ExitCode] = (for {
    sumPar <- sumProgramParallel
    _ <- printLine(s"Parallel sum: $sumPar")
  } yield ExitCode.success)
    .catchAll(e => printLine(s"Error: ${e.getMessage}").as(ExitCode.failure))
    .provide(ZLayer.succeed(ConsoleLive), ZLayer.succeed(RandomLive), ZLayer.succeed(ClockLive))

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */

  trait Profiler {
    def printEffectRunningTime[R, E, A](label: String)(zio: ZIO[R, E, A]): ZIO[R with Console with Clock, E, A]
  }

  private case class ProfilerLive(console: Console, clock: Clock) extends Profiler {
    override def printEffectRunningTime[R, E, A](label: String)(zio: ZIO[R, E, A]): ZIO[R, E, A] =
      for {
        start <- clock.currentTime(TimeUnit.MILLISECONDS)
        result <- zio
        end <- clock.currentTime(TimeUnit.MILLISECONDS)
        _ <- console.printLine(s"[$label] Execution time: ${end - start} ms").orDie
      } yield result
  }

  private object ProfilerLive {
    val layer: URLayer[Console with Clock, Profiler] =
         ZLayer.fromFunction(ProfilerLive(_, _))
      // ZLayer.fromFunction(ProfilerLive(_: Console, _: Clock))
      // ZLayer.fromFunction((console: Console, clock: Clock) => ProfilerLive(console: Console, clock: Clock))
  }

  private object Profiler {
    val live: URLayer[Console with Clock, Profiler] = ProfilerLive.layer
  }

  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: ZIO[Random with Clock with Console with Profiler, Throwable, ExitCode] = for {
    profiler <- ZIO.service[Profiler]

    seqSum <- profiler.printEffectRunningTime("Sequential sum") {
      ZIO.collectAll(effects).map(_.sum)
    }
    _ <- printLine(s"Sequential sum: $seqSum")

    parSum <- profiler.printEffectRunningTime("Parallel sum") {
      ZIO.collectAllPar(effects).map(_.sum)
    }
    _ <- printLine(s"Parallel sum: $parSum")
  } yield ExitCode.success

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp: ZIO[Any, Throwable, ExitCode] = appWithTimeLogg
    .provide(Profiler.live, ZLayer.succeed(ConsoleLive), ZLayer.succeed(RandomLive), ZLayer.succeed(ClockLive))

}
