package ru.otus.module3.zio_homework

import zio.{ExitCode, ZIO, ZIOAppDefault}

object ZioHomeWorkApp extends ZIOAppDefault {

  //override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ru.otus.module3.zio_homework.guessProgram
  //override def run: ZIO[Any, Throwable, ExitCode] = ru.otus.module3.zio_homework.app
  //override def run: ZIO[Any, Throwable, ExitCode] = ru.otus.module3.zio_homework.appSpeedUp
  override def run: ZIO[Any, Throwable, ExitCode] = ru.otus.module3.zio_homework.runApp


}
