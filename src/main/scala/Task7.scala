import scala.io.BufferedSource
import scala.math

import scala.math.pow
implicit class PowerInt(i: Int) {
  def **(b: Int): Int = pow(i, b).intValue()
}
object Task7 {

  case class Eqs(result: Long, inputs:List[Long]) {
    def eval(): Boolean = {
      val n = 2 ** (inputs.size-1)
      !(0 until n).forall { i =>
        val sBin = i.toBinaryString
        val trailingZeroNr = inputs.size - 1 - sBin.length
        val it = "0".repeat(trailingZeroNr).concat(sBin).iterator
        val res = inputs.reduce { (a,b) => it.next() match
          case '0' => a + b
          case '1' => a * b
        }
        res != result
      }
    }

    def eval3(): Boolean = {
      val n = 3 ** (inputs.size - 1)
      !(0 until n).forall { i =>
        val sBin = Integer.toString(i, 3)
        val trailingZeroNr = inputs.size - 1 - sBin.length
        val it = "0".repeat(trailingZeroNr).concat(sBin).iterator
        val res = inputs.reduce { (a, b) =>
          it.next() match
            case '0' => a + b
            case '1' => a * b
            case '2' => a.toString.concat(b.toString).toLong
        }
        res != result
      }

    }
  }

  def parseLine(line: String): Eqs = {
    val splitted = line.split(':')
    val inputs = splitted(1).drop(1).split(' ').map(_.toLong)
    Eqs(splitted(0).toLong, inputs.toList)
  }

  def parseFile1(file: BufferedSource): Long = {
    val eqs = file.getLines().map(parseLine)
    eqs.filter(_.eval()).map { eq => eq.result }.sum()
  }

  def parseFile2(file: BufferedSource): Long= {
    val eqs = file.getLines().map(parseLine)
    eqs.filter(_.eval3()).map { eq => eq.result }.sum()
  }
}
