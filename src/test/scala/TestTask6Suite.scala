import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask6Suite extends AnyFunSuite {
  test("task6_1 test file") {
    assert(Task6.parseFile1(Source.fromResource("input_test_6.txt")) == 41)
  }

  test("task6_1 all file") {
    assert(Task6.parseFile1(Source.fromResource("input_all_6.txt")) == 4890)
  }

  test("task6_2 test file") {
    assert(Task6.parseFile2(Source.fromResource("input_test_6.txt")) == 6)
  }

  test("task6_2 all file") {
    assert(Task6.parseFile2(Source.fromResource("input_all_6.txt")) == 1995)
  }
}
