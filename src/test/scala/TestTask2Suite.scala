import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask2Suite extends AnyFunSuite {
  test("task2_1 test file") {
    assert(Task2.parseFile1(Source.fromResource("input_test_2.txt")) == 2)
  }

  test("task2_1 all file") {
    assert(Task2.parseFile1(Source.fromResource("input_all_2.txt")) == 606)
  }

  test("task2_2 test file") {
    assert(Task2.parseFile2(Source.fromResource("input_test_2.txt")) == 4)
  }

  test("task2_2 all file") {
    assert(Task2.parseFile2(Source.fromResource("input_all_2.txt")) == 644)
  }
}
