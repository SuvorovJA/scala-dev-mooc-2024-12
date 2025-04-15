package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import ru.otus.module3.catsconcurrency.cats_effect_homework.Wallet._

import java.nio.file.{Files, Paths}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]

  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]

  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]

  // id кошелька
  def id(): WalletId
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкурентного доступа и производительности не заморачиваемся,
// делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_] : Sync](id: WalletId) extends Wallet[F] {
  private val filePath = Paths.get(s"wallet_$id.txt")

  private def writeBalance(balance: BigDecimal): F[Unit] = {
    Sync[F].delay(Files.write(filePath, balance.toString().getBytes()))
  }

  private def readBalance(): F[BigDecimal] = {
    if (Files.exists(filePath)) {
      Sync[F].delay(BigDecimal(Files.readString(filePath)))
    } else {
      Sync[F].pure(BigDecimal("0"))
    }
  }

  override def balance: F[BigDecimal] = readBalance()

  override def topup(amount: BigDecimal): F[Unit] = for {
    currentBalance <- readBalance()
    newBalance = currentBalance + amount
    _ <- writeBalance(newBalance)
  } yield ()

  override def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    currentBalance <- readBalance()
    _ <- if (currentBalance >= amount) {
      val newBalance = currentBalance - amount
      writeBalance(newBalance).map(Right(_))
    } else {
      Sync[F].pure(Left(BalanceTooLow))
    }.handleErrorWith(_ => Sync[F].pure(Left(BalanceTooLow)))
  } yield Right(())

  override def id(): WalletId = id
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_] : Sync](id: WalletId): F[Wallet[F]] = {
    val wallet = new FileWallet[F](id)

    Sync[F].delay(wallet)
  }

  type WalletId = String

  sealed trait WalletError

  case object BalanceTooLow extends WalletError
}
