import scala.io.BufferedSource

object Utils {
  def fileToCharArray(file: BufferedSource): Array[Array[Char]] = {
    val lines = file.getLines().toList
    (for {
      line <- lines
    } yield line.toCharArray).toArray
  }

  case class Cord(r: Int, c: Int) {
    def +(that: Cord): Cord = Cord(r + that.r, c + that.c)

    def -(that: Cord): Cord = Cord(r - that.r, c - that.c)

    def *(that: Cord): Int = r * that.r + c * that.c

    def valid(maxRowIdx: Int, maxColIdx: Int): Boolean = (r <= maxRowIdx) && (c <= maxColIdx) && (r > -1) && (c > -1)

    def *(const: Int): Cord = Cord(r * const, c * const)
  }

}
