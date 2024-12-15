import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask8Suite extends AnyFunSuite {
  val taskNr = 8
  val task: Task8.type = Task8

  test("task 1st test file") {
    assert(task.parseFile1(Source.fromResource(s"input_test_$taskNr.txt")) == 14)
  }

  test("task 1st all file") {
    assert(task.parseFile1(Source.fromResource(s"input_all_$taskNr.txt")) == 220)
  }

  test("task 2nd test file") {
    assert(task.parseFile2(Source.fromResource(s"input_test_$taskNr.txt")) == 34)
  }

  test("task 2nd all file") {
    assert(task.parseFile2(Source.fromResource(s"input_all_$taskNr.txt")) == 813)
  }
}
