import scala.annotation.tailrec
import scala.io.BufferedSource

object Task2 {
  def parseFile1(file: BufferedSource): Int = {
    file.getLines().count { line => evalReport(line.split(" ")) }
  }

  def evalReport(levels: Array[String]): Boolean = {
    val itr = levels.iterator
    val first = itr.next().toInt
    val second = itr.next().toInt
    val diff = second - first
    if(math.abs(diff) > 3 || diff == 0) return false
    evalPairs(second, itr, math.signum(diff))
  }

  @tailrec
  def evalPairs(curr: Int, itr:Iterator[String], sign: Int): Boolean = {
    val nextLevel = itr.next().toInt
    val diff = nextLevel - curr

    if (diff*sign > 3 || diff*sign <= 0 ) return false
    if (!itr.hasNext) return true

    evalPairs(nextLevel, itr, sign)
  }

  def parseFile2(file: BufferedSource): Int = {
    file.getLines().count { line => evalReportWithDampener(line.split(" ")) }
  }

  def evalReportWithDampener(levels: Array[String]): Boolean = {
    val itr = levels.iterator
    val first = itr.next().toInt
    val second = itr.next().toInt
    val diff = second - first
    val third = itr.next().toInt
    val diff2 = third - first
    val diff3 = third - second

    (math.abs(diff) <= 3 && diff != 0 && evalPairsWithDampener(second, levels.drop(2).iterator, math.signum(diff), damped = false)) ||
      (math.abs(diff2) <= 3 && diff2 != 0 && evalPairsWithDampener(third, levels.drop(3).iterator, math.signum(diff2), damped = true)) ||
      (math.abs(diff3) <= 3 && diff3 != 0 && evalPairsWithDampener(third, levels.drop(3).iterator, math.signum(diff3), damped = true))
  }

  @tailrec
  def evalPairsWithDampener(curr: Int, itr:Iterator[String], sign: Int, damped: Boolean): Boolean = {
    val nextLevel = itr.next().toInt
    val diff = nextLevel - curr

    val (lvl, dUpd) = if (diff*sign > 3 || diff*sign <= 0 )
      if (damped) return false
      else (curr, true)
    else (nextLevel, damped)
    if (!itr.hasNext) return true

    evalPairsWithDampener(lvl, itr, sign, dUpd)
  }
}
