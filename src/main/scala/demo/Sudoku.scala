package demo


object Sudoku {

  type T = (Int, Int) => Seq[Int]

  def solve(s: Seq[T]): Seq[T] = {
    def search(l: Seq[T], p: (Int, Int)): Seq[T] = for {
      s <- l
      n <- s(p._1, p._2)
    } yield {
      mark((p, n), s)
    }
    idx().foldLeft(s)((acc, ij) => search(acc, ij))
  }

  def e(a: Int, b: Int): Boolean = (a - 1) / 3 == (b - 1) / 3

  def mark(pn: ((Int, Int), Int), s: T): T = {
    val (p, n) = pn
    val (i, j) = p
    val filtered: (Int, Int) => Seq[Int] = { (x, y) =>
      val q = (x, y)
      if (p == q) List(n)
      else if (x == i || y == j || (e(x, i) || e(y, j))) s(q._1, q._2).filter(_ != n)
      else s(q._1, q._2)
    }
    filtered
  }

  def disp(s: T): String = (for {
    i <- 1 to 9
    j <- 1 to 9
  } yield {
    s(i, j).head.toString
  }).mkString(" ")

  def input(s: String): T = {
    val ws = for {
      l <- s.split("\n")
      w <- l.split(" ")
    } yield w.toInt

    val ii: (Int, Int) => Seq[Int] = { (_, _) => 1 to 9}

    idx.zip(ws).filter(_._2 > 0).foldLeft(ii)((acc, pn) => mark(pn, acc))

  }

  def idx(): Seq[(Int, Int)] = for (i <- 1 to 9; j <- 1 to 9) yield (i, j)

  def getContents(): Seq[String] = List(
    "0 5 0 0 6 0 0 0 1",
    "0 0 4 8 0 0 0 7 0",
    "8 0 0 0 0 0 0 5 2",
    "2 0 0 0 5 7 0 3 0",
    "0 0 0 0 0 0 0 0 0",
    "0 3 0 6 9 0 0 0 5",
    "7 9 0 0 0 0 0 0 8",
    "0 1 0 0 0 6 5 0 0",
    "5 0 0 0 3 0 0 6 0"
  )


  def main(args: scala.Array[scala.Predef.String]): scala.Unit = {
    for {
      s <- getContents()
    } {
      println(solve(List(input(s))).map(disp).mkString("\n"))
    }
  }

}
