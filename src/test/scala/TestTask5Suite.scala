import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask5Suite extends AnyFunSuite {
  test("task5_1 test file") {
    assert(Task5.parseFile1(Source.fromResource("input_test_5.txt")) == 143)
  }

  test("task5_1 all file") {
    assert(Task5.parseFile1(Source.fromResource("input_all_5.txt")) == 6041)
  }

  test("task5_2 test file") {
    assert(Task5.parseFile2(Source.fromResource("input_test_5.txt")) == 123)
  }

  test("task5_2 all file") {
    assert(Task5.parseFile2(Source.fromResource("input_all_5.txt")) == 4884)
  }
}
