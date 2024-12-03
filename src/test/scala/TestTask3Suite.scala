import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class TestTask3Suite extends AnyFunSuite {
  test("task3_1 test file") {
    assert(Task3.parseFile1(Source.fromResource("input_test_3.txt")) == 161)
  }

  test("task3_1 all file") {
    assert(Task3.parseFile1(Source.fromResource("input_all_3.txt")) == 181345830)
  }

  test("task3_2 test file") {
    assert(Task3.parseFile2(Source.fromResource("input_test_3b.txt")) == 48)
  }

  test("task3_2 all file") {
    assert(Task3.parseFile2(Source.fromResource("input_all_3.txt")) == 98729041)
  }
}
