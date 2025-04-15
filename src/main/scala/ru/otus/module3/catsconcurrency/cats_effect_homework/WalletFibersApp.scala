package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._
import scala.concurrent.duration._

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def refillWallet(wallet: Wallet[IO], interval: FiniteDuration): IO[Unit] = {
    def loop: IO[Unit] = wallet.topup(100) >> IO.sleep(interval) >> loop
    loop
  }

  def printBalancesPeriodically(wallets: Seq[Wallet[IO]], interval: FiniteDuration): IO[Unit] = {
    def loop: IO[Unit] =
      wallets
        .traverse(w => w.balance.map(b => (w, b)))
        .flatMap { balances =>
          IO.print("Current balances:") >> balances.traverse_ { case (w, b) =>
              IO.print(s" Wallet ${w.id()}: $b |")
            }
        } >> IO.println("") >> IO.sleep(interval) >> loop
    loop
  }

  def run: IO[Unit] =
    for {

      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")

      // Запускаем фоновые процессы для пополнения кошельков
      fiber1 <- refillWallet(wallet1, 100.millis).start
      fiber2 <- refillWallet(wallet2, 500.millis).start
      fiber3 <- refillWallet(wallet3, 2000.millis).start

      // Запускаем фоновый процесс для вывода балансов на экран
      fiber4 <- printBalancesPeriodically(Seq(wallet1, wallet2, wallet3), 1.second).start

      // Ждем ввода пользователя
      _ <- IO.println("Press any key to stop...")
      _ <- IO.readLine

      // Отменяем все фоновые процессы
      _ <- fiber1.cancel
      _ <- fiber2.cancel
      _ <- fiber3.cancel
      _ <- fiber4.cancel

    } yield ()

}

//C:\bin\java\Axiom17\bin\java.exe ...
//Press any key to stop...
//Current balances: Wallet 1: 47300 | Wallet 2: 10900 | Wallet 3: 2900 |
//Current balances: Wallet 1: 48100 | Wallet 2: 11100 | Wallet 3: 2900 |
//Current balances: Wallet 1: 49000 | Wallet 2: 11300 | Wallet 3: 3000 |
//Current balances: Wallet 1: 49900 | Wallet 2: 11500 | Wallet 3: 3000 |
//Current balances: Wallet 1: 50700 | Wallet 2: 11700 | Wallet 3: 3100 |
//Current balances: Wallet 1: 51600 | Wallet 2: 11900 | Wallet 3: 3100 |
//Current balances: Wallet 1: 52500 | Wallet 2: 12100 | Wallet 3: 3200 |
//Current balances: Wallet 1: 53400 | Wallet 2: 12300 | Wallet 3: 3200 |
//Current balances: Wallet 1: 54300 | Wallet 2: 12500 | Wallet 3: 3300 |
//
//
//Process finished with exit code 0