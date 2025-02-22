case class LocaleConfig(lineSeparator: String, valueSeparator: Char)

case class Car(year: Int, mark: String, model: String, canDrive: Boolean)

class MonadParser[T, Src](private val p: Src => (T, Src)):

  def flatMap[M](f: T => MonadParser[M, Src]): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      f(word).p(rest)
    }

  def map[M](f: T => M): MonadParser[M, Src] =
    MonadParser { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }

  def parse(src: Src): T = p(src)._1

object MonadParser:

  def apply[T, Src](f: Src => (T, Src)) = new MonadParser(f)

class FieldConverters(using config: LocaleConfig):

  def stringField: MonadParser[String, String] =
    MonadParser { src =>
      val idx = src.indexOf(config.valueSeparator)
      if idx >= 0 then (src.take(idx), src.drop(idx + 1))
      else (src, "")
    }

  def intField: MonadParser[Int, String] = stringField.map(_.toInt)

  def booleanField: MonadParser[Boolean, String] = stringField.map(_.toBoolean)

object FieldConverters:
  given LocaleConfig =
    val lineSeparator = System.lineSeparator()
    val valueSeparator = java.text.DecimalFormatSymbols(java.util.Locale.getDefault())
      .getPatternSeparator
    LocaleConfig(lineSeparator, valueSeparator)

  private given FieldConverters = FieldConverters()

  val parsers: FieldConverters = summon[FieldConverters]

object TestExecutor:

  private val testData = "1997;Ford;Passat;true\r\n1901;Ford;T;false\r\n1971;ZAZ;968;false"

  import FieldConverters.parsers.*
  import FieldConverters.given_LocaleConfig

  def main(args: Array[String]): Unit =

    val carParser = for
      year <- intField
      mark <- stringField
      model <- stringField
      canDrv <- booleanField
    yield Car(year, mark, model, canDrv)

    val result = testData.split(summon[LocaleConfig].lineSeparator).map(carParser.parse)

    println(result.mkString(summon[LocaleConfig].lineSeparator))