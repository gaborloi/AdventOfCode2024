import Utils.{Cord, fileToCharArray}

import scala.annotation.tailrec
import scala.io.BufferedSource
import scala.language.implicitConversions

implicit def bool2int(b:Boolean): Int = if (b) 1 else 0

object Task6 {
  case class GuardPathFinder(room: Array[Array[Char]]) {
    val startCord: Cord = {
      val rowStart = room.indexWhere { arr => arr.contains('^') }
      val colStart = room(rowStart).indexOf('^')
      Cord(rowStart, colStart)
    }
    val maxRow: Int = room.length - 1
    val maxCol: Int = room(0).length - 1

    def valueOf(cord: Cord): Option[Char] = if (cord.valid(maxRow, maxCol)) Some(room(cord.r)(cord.c)) else None
    @tailrec
    final def visitedPlaces(currPos:Cord, dir:Cord, visitedCords: List[(Cord, Cord)]): List[(Cord, Cord)] = {
      val cordToEval = currPos + dir
      val (nextCord, nextDir, nextVisitedCords) = valueOf(cordToEval) match
        case Some('#') => (currPos, dir.rotateRight(), visitedCords)
        case None => return visitedCords :+ (currPos, dir)
        case _ => (cordToEval, dir, visitedCords :+ (currPos, dir))
      visitedPlaces(nextCord, nextDir, nextVisitedCords)
    }

    @tailrec
    final def checkForLoop(
      currPos: Cord, dir: Cord, specialCord: Cord, visitedCords: Set[(Cord, Cord)]
    ): Boolean = {
      val cordToEval = currPos + dir

      val (nextCord, nextDir) = if (cordToEval == specialCord) (currPos, dir.rotateRight()) else valueOf(cordToEval) match
        case Some('#') => (currPos, dir.rotateRight())
        case None => return false
        case _ => (cordToEval, dir)

      if (visitedCords.contains((nextCord, nextDir))) return true
      val visitedCordsUpd = visitedCords.union(Set((nextCord, nextDir)))

      checkForLoop(nextCord, nextDir, specialCord, visitedCordsUpd)
    }
  }

  def parseFile1(file: BufferedSource): Int = {
    val room = fileToCharArray(file)
    val gpf = GuardPathFinder(room)
    gpf.visitedPlaces(gpf.startCord, Cord(-1,0), List[(Cord, Cord)]()).map {_._1}.toSet.size
  }

  def parseFile2(file: BufferedSource): Int = {
    val room = fileToCharArray(file)
    val gpf = GuardPathFinder(room)
    val visitedPlaces = gpf.visitedPlaces(gpf.startCord, Cord(-1,0), List[(Cord, Cord)]())
    val cordsToCheck = visitedPlaces.map { _._1 }.toSet.diff(Set(gpf.startCord))

    cordsToCheck.foldLeft(0) { (s,c) =>
      val hist = visitedPlaces.takeWhile(_._1 != c)
      s + gpf.checkForLoop(hist.last._1, hist.last._2, c, hist.toSet)
    }
  }
}
