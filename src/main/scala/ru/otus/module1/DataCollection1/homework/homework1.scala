package ru.otus.module1.DataCollection1.homework

import scala.util.Random

class BallsExperiment {
  val urn: List[Int] = List(1, 0, 1, 0, 1, 0) // 1-white 0-black
  val random = new Random()

  // В урне 3 белых и 3 черных шара. Из урны дважды вынимают по одному шару,
  // не возвращая их обратно. Найти вероятность появления белого шара при втором испытании (событие В),
  // если при первом испытании был извлечен черный шар (событие А).
  def isFirstBlackSecondWhite(): Boolean = {
    val takeIndexOne = random.nextInt(urn.length)
    val firstBall = urn(takeIndexOne)
    val remainingUrn = urn.take(takeIndexOne) ++ urn.drop(takeIndexOne + 1)
    val secondBall = remainingUrn(random.nextInt(remainingUrn.length))
    firstBall == 0 && secondBall == 1
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val etalon = 3 / 5.0 // Искомая условная вероятность
    val pa = 0.5 //вероятность появления белого шара при первом испытании

    Seq.range(1, 11).foreach(i => {
      val listOfExperiments: List[BallsExperiment] = List.fill(count)(new BallsExperiment)
      val countOfExperiments: Seq[Boolean] = listOfExperiments.map(_.isFirstBlackSecondWhite())
      val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
      val pab = countOfPositiveExperiments / count
      val pba = pab / pa // формула для условной вероятности
      println(s"$i:  $etalon - $pba ~= ${etalon - pba}")
      //1:  0.6 - 0.6046000123023987 ~= -0.004600012302398704
      //2:  0.6 - 0.5979999899864197 ~= 0.0020000100135803
      //3:  0.6 - 0.603600025177002 ~= -0.0036000251770019753
      //4:  0.6 - 0.5920000076293945 ~= 0.007999992370605447
      //5:  0.6 - 0.592199981212616 ~= 0.007800018787384011
      //6:  0.6 - 0.5992000102996826 ~= 7.999897003173606E-4
      //7:  0.6 - 0.6046000123023987 ~= -0.004600012302398704
      //8:  0.6 - 0.6074000000953674 ~= -0.007400000095367454
      //9:  0.6 - 0.6146000027656555 ~= -0.01460000276565554
      //10:  0.6 - 0.5950000286102295 ~= 0.004999971389770486
    })
  }
}