package puzzles

import puzzles.TwoDimArray.TwoDimStringArray

/**
  * Created by dmohan200 on 11/23/16.
  */
object TwoDimArray {

  type TwoDimStringArray = Array[Array[String]]

  def apply(row: Int, col: Int): TwoDimArray = {
    assert(row > 0, "row count must be greater than 0")
    assert(col > 0, "row count must be greater than 0")
    val twoDim = Array.ofDim[String](row, col)
    for {
      i ← 0 until row
      j ← 0 until col
    } yield {
      twoDim(i)(j) = s"$i,$j"
    }
    TwoDimArray(twoDim)
  }
}

case class TwoDimArray(array: TwoDimStringArray) {

  val rows = array.length
  val cols = array(0).length

  override def toString = {
    var buff = "\n"
    for {
      i ← 0 until rows
      j ← 0 until cols
    } yield {
      buff = buff + array(i)(j) + "|"
      if(j + 1 == cols) buff = buff + "\n"
    }
    buff
  }

  def traverse() = {
    left(0, cols)

    def left(x: Int, y: Int): Unit = {
      print(s"\n($x, $y) left  => ")
      for {
        j ← y - 1 to cols - y by -1
      } yield {
        print(array(x)(j) + "|")
      }
      if (rows/2 != x) down(x, x)
    }

    def down(x: Int, y: Int) = {
      print(s"\n($x, $y) down  => ")
      for {
        i ← x + 1 until (rows - x)
      } yield {
        print(array(i)(y) + "|")
      }
      if (cols/2 != y) right(rows - 1 - x, y)
    }

    def right(x: Int, y: Int) = {
      print(s"\n($x, $y) right => ")
      for {
        j ← y + 1 until (cols - y)
      } yield {
        print(array(x)(j) + "|")
      }
      if (rows/2 != x) up(x, cols - 1 - y)
    }

    def up(x: Int, y: Int) = {
      print(s"\n($x, $y) up    => ")
      for {
        i ← (x - 1) to (rows - x) by -1
      } yield {
        print(array(i)(y) + "|")
      }
      if (cols/2 != y) left(rows - x, y)
    }
  }
}

object TwoDimArrayApp extends App {
    override def main(args: Array[String]): Unit = {
      // try TwoDimArray(10, 10), TwoDimArray(10, 5), TwoDimArray(10, 1), TwoDimArray(1, 10), TwoDimArray(1, 1)
      val a3 = TwoDimArray(5, 10)
      println("=======")
      println(a3)
      println("=======")
      a3.traverse()
    }
}
