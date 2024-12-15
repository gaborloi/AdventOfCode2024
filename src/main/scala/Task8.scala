import Utils.{Cord, fileToCharArray}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Task8 {
  def mirrorCord(cord:Cord, mirror: Cord): Cord = mirror * 2 - cord

  def createAntennaMap(antennas: Array[Array[Char]]): Map[Char, List[Cord]] =
    antennas.indices.foldLeft(Map[Char, List[Cord]]()) { (m, i) =>
      antennas(i).indices.foldLeft(m) { (mm, j) =>
        antennas(i)(j) match
          case '.' => mm
          case c: Char => if (mm.contains(c)) {
            mm.updated(c, mm(c) :+ Cord(i, j))
          } else {
            mm.updated(c, List(Cord(i, j)))
          }
      }
    }

  def parseFile1(file: BufferedSource): Long = {
    val antennas = fileToCharArray(file)
    val antennaMap = createAntennaMap(antennas)
    antennaMap.foldLeft(Set[Cord]()) { case (s, (_, cords)) =>
        cords.combinations(2).toList.foldLeft(s) { (ss, lc) =>
          ss.union(Set(mirrorCord(lc.head, lc(1)), mirrorCord(lc(1), lc.head)))
        }
      }.count { c => c.valid(antennas.length - 1, antennas.head.length - 1) }
  }

  @tailrec
  def findAntinodes(cord: Cord, dir: Cord, dim: Int, antinodes: Set[Cord]): Set[Cord] = {
    val newCord = cord + dir
    if (!newCord.valid(dim, dim)) return antinodes
    findAntinodes(newCord, dir, dim, antinodes + newCord)
  }
  def parseFile2(file: BufferedSource): Int = {
    val antennas = fileToCharArray(file)
    val antennaMap = createAntennaMap(antennas)
    val d = antennas.length - 1

    antennaMap.foldLeft(Set[Cord]()) { case (s, (_, cords)) =>
      cords.combinations(2).toList.foldLeft(s) { (ss, lc) =>
        ss.union(findAntinodes(lc(1), lc(1) - lc.head, d, Set[Cord](lc(1))))
          .union(findAntinodes(lc.head, lc.head - lc(1), d, Set[Cord](lc.head)))
      }
    }.size
  }
}
