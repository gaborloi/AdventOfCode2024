import scala.io.BufferedSource
import Utils._

import scala.annotation.tailrec

object Task4 {

  private val steps = {
    List[Cord](Cord(0, 1), Cord(0, -1), Cord(1, 0), Cord(-1, 0), Cord(1, 1), Cord(-1, -1), Cord(1, -1), Cord(-1, 1))
  }
  private val diagSteps = List[Cord](Cord(1,1), Cord(1,-1))

  case class Puzzle(file:BufferedSource) {
    private val puzzle: Array[Array[Char]] = fileToCharArray(file)
    private val rowIndices = puzzle.indices
    private val colIndices = puzzle(0).indices
    private val rowMax = rowIndices.last
    private val colMax = colIndices.last

    def getValue(cord:Cord): Char = puzzle(cord.r)(cord.c)

    def countXmas(): Int = rowIndices.foldLeft(0) { (s, r) =>
      colIndices.foldLeft(s) { (ss, c) =>
        puzzle(r)(c) match {
          case 'X' => steps.foldLeft(ss) { (sss, cord) =>
            sss + checkDir(Cord(r, c), cord, "MAS".iterator)
          }
          case _ => ss
        }
      }
    }
    @tailrec
    final def checkDir(pos: Cord, dir:Cord, xmasCounter: Iterator[Char]): Int = {
      val newPos = pos + dir
      if (!newPos.valid(rowMax, colMax) || getValue(newPos) != xmasCounter.next()) return 0
      if (!xmasCounter.hasNext) return 1
      checkDir(newPos, dir, xmasCounter)
    }

    def countX_Mas(): Int = rowIndices.foldLeft(0) { (s, r) =>
      colIndices.foldLeft(s) { (ss, c) =>
        puzzle(r)(c) match {
          case 'A' => if(diagSteps.forall { d => checkDiag(Cord(r,c),d) }) ss + 1 else ss
          case _ => ss
        }
      }
    }

    def checkDiag(pos: Cord, dir:Cord): Boolean = {
      val pPos = pos + dir
      val nPos = pos - dir
      pPos.valid(rowMax, colMax) && nPos.valid(rowMax, colMax) &&
        List(getValue(pPos), getValue(nPos),'A').sorted.mkString == "AMS"
    }
  }

  def parseFile1(file: BufferedSource): Int = {
   val p = Puzzle(file)
    p.countXmas()
  }

  def parseFile2(file: BufferedSource): Int = {
    val p = Puzzle(file)
    p.countX_Mas()
  }
}
