import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask7Suite extends AnyFunSuite {
  val taskNr = 7
  val task: Task7.type = Task7

  test("task 1st test file") {
    assert(task.parseFile1(Source.fromResource(s"input_test_$taskNr.txt")) == 3749L)
  }

  test("task 1st all file") {
    assert(task.parseFile1(Source.fromResource(s"input_all_$taskNr.txt")) == 267566105056L)
  }

  test("task 2nd test file") {
    assert(Task7.parseFile2(Source.fromResource(s"input_test_$taskNr.txt")) == 11387L)
  }

  test("task 2nd all file") {
    assert(Task7.parseFile2(Source.fromResource(s"input_all_$taskNr.txt")) == 116094961956019L)
  }
}
