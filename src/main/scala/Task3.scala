import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.util.matching.Regex

object Task3 {
  val mulPattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
  val crossPattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)""".r
  def parseFile1(file: BufferedSource): Int = {
    file.getLines().map(parseLine).sum()
  }

  def parseLine(s: String): Int = {
    val matches = mulPattern.findAllMatchIn(s)
    matches.foldLeft(0) { case (s,m) =>
      s + m.group(1).toInt * m.group(2).toInt
    }
  }

  def parseFile2(file: BufferedSource): Int = {
    val s = file.getLines().toList.reduce{ (a: String, b:String) => a.concat(b) }
    parseLineWithDo(s)
  }

  def parseLineWithDo(s: String): Int = {
    val matches = crossPattern.findAllMatchIn(s)
    calculateMulWithDo(matches, toBeCalced = true, 0)
  }
  @tailrec
  def calculateMulWithDo(itr: Iterator[Regex.Match], toBeCalced: Boolean, value: Int): Int = {
    val m = itr.next()
    val (vNew, bNew) = m.group(0) match {
      case "do()" => (value, true)
      case "don't()" => (value, false)
      case _ => if (toBeCalced) (value + m.group(1).toInt * m.group(2).toInt, toBeCalced) else (value, toBeCalced)
    }

    if(!itr.hasNext) return vNew
    calculateMulWithDo(itr, bNew, vNew)
  }
}
