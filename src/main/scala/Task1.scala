import scala.io.BufferedSource

object Task1 {
  def parseFile1(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val firstOrderedList = lines.map { line =>
      line.split(" ").head.toInt
    }.sorted
    val secondOrderedList = lines.map { line =>
      line.split(" ").last.toInt
    }.sorted
    firstOrderedList.zip(secondOrderedList).foldLeft(0: Int) { case (s: Int, (a: Int,b: Int)) => s + math.abs(a - b) }
  }

  def parseFile2(file: BufferedSource): Int = {
    val lines = file.getLines().toList
    val firstOrderedList = lines.map { line =>
      line.split(" ").head.toInt
    }.sorted
    val secondOrderedList = lines.map { line =>
      line.split(" ").last.toInt
    }.sorted
    firstOrderedList.foldLeft(0: Int) { case (s: Int, a:Int) => s + a * secondOrderedList.count(_ == a) }
  }
}
