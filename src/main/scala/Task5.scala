import Utils._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

object Task5 {

  def parseFile1(file: BufferedSource): Int = {
    val it = file.getLines()
    val rules = getRuleMap(it, Map())
    val listOfPages = it.map(_.split(',').map(_.toInt).toList).toList
    sumValids(rules, listOfPages)
  }


  def sumValids(rules: Map[Int, Set[Int]], listOfPages: List[List[Int]]): Int = {
    val valids: List[List[Int]] = listOfPages.filter { pages => evalPage(pages.iterator, Set(), Set(), rules) }
    valids.map( pages => pages(pages.size/2) ).sum()
  }

  @tailrec
  def evalPage(pageIt: Iterator[Int], invalids: Set[Int], previousPages:Set[Int], rules: Map[Int, Set[Int]]): Boolean = {
    val currPage = pageIt.next()
    if (invalids.contains(currPage)) return false
    if(!pageIt.hasNext) return true
    val invalidsUpd = invalids.union(rules.getOrElse(currPage, Set[Int]()).diff(previousPages))
    val previousPagesUpd = previousPages + currPage
    evalPage(pageIt,invalidsUpd, previousPagesUpd, rules)
  }

  @tailrec
  def getRuleMap(it: Iterator[String], m:Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
    val rule = it.next()
    if(rule.isEmpty) return m
    val mUpd = rule.split('|').map(_.toInt) match {
      case Array(l,u) => if(m.contains(u)) m.updated(u, m(u) + l) else m.updated(u, Set(l))
    }
    getRuleMap(it, mUpd)
  }


  def parseFile2(file: BufferedSource): Int = {
    val it = file.getLines()
    val rules = getRuleMap(it, Map())
    val listOfPages = it.map(_.split(',').map(_.toInt).toList).toList
    sumInvalids(rules, listOfPages)
  }

  def sortPages(pages: List[Int], rules: Map[Int, Set[Int]]): List[Int] = {
    pages.sortWith { (a, b) => rules.getOrElse(a, Set()).contains(b)}
  }

  def sumInvalids(rules: Map[Int, Set[Int]], listOfPages: List[List[Int]]): Int = {
    val invalids: List[List[Int]] = listOfPages.filterNot { pages => evalPage(pages.iterator, Set(), Set(), rules) }
    invalids.map( pages => sortPages(pages, rules)(pages.size/2) ).sum()
  }
}
