package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._

object WalletMain extends IOApp.Simple {

  def run: IO[Unit] =
    for {
      wallet <- Wallet.fileWallet[IO]("test_wallet")
      _ <- wallet.topup(100.0)
      _ <- wallet.balance.flatMap(IO.println)
      _ <- wallet.withdraw(50.0)
      _ <- wallet.balance.flatMap(IO.println)
    } yield ()

}

//C:\bin\java\Axiom17\bin\java.exe ...
//100.0
//50.0
//
//Process finished with exit code 0
//
//C:\bin\java\Axiom17\bin\java.exe ...
//150.0
//100.0
//
//Process finished with exit code 0