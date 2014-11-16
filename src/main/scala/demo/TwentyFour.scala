package demo

import scala.collection.immutable.IndexedSeq


object TwentyFour extends App {

  //println("Please input fours numbers: separate by ,")
  //val numbers = StdIn.readLine().split(",").map(_.toDouble)
  //assert(numbers.length == 4, "the length must be 4")

  def calc(express: String, res:Double, nums: Double*): Seq[(String, Double)] = {

    nums.toList match {
      case x :: Nil => List(
        (s"(${x.toInt} + $express)", x + res),
        (s"(${x.toInt} - $express)", x - res),
        (s"($express - ${x.toInt})", res - x),
        (s"${x.toInt} * $express", x * res),
        (s"${x.toInt} / $express", x / res),
        (s"$express / ${x.toInt}", res / x)
      )
      case x :: y :: Nil => calc(express, res, x).flatMap(res => calc(res._1, res._2, y))
      case x :: y :: z :: Nil =>
        calc(express, res, x, y).flatMap(res => calc(res._1, res._2, z)) ++
          calc(express, res, x, z).flatMap(res => calc(res._1, res._2, y)) ++
          calc(express, res, y, z).flatMap(res => calc(res._1, res._2, x))
    }

  }
  //calc(numbers.head.toString, numbers.head, numbers.tail: _*).filter(_._2 == 24).distinct.foreach(res => println(res._1))

  val numss = for {
    a <- 1 to 10
    b <- 1 to 10
    c <- 1 to 10
    d <- 1 to 10
  } yield (a,b,c,d)
  val res: IndexedSeq[Seq[String]] = numss.map(nums => calc(nums._1.toString, nums._1, nums._2,nums._3, nums._4).filter(_._2 == 24).map(_._1).distinct).filter(ls => ls.size == 1)
  res.flatten.foreach(println)
}
