import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask4Suite extends AnyFunSuite {
  test("task4_1 test file") {
    assert(Task4.parseFile1(Source.fromResource("input_test_4.txt")) == 18)
  }

  test("task4_1 all file") {
    assert(Task4.parseFile1(Source.fromResource("input_all_4.txt")) == 2454)
  }

  test("task4_2 test file") {
    assert(Task4.parseFile2(Source.fromResource("input_test_4.txt")) == 9)
  }

  test("task4_2 all file") {
    assert(Task4.parseFile2(Source.fromResource("input_all_4.txt")) == 1858)
  }
}
