package fuzzy

import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.HashMap
import math.pow

/**
 * This class is a test suite for the methods in package fuzzy.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FuzzySuite extends AnyFunSuite {

  trait TestMemberships {
    val trapezoidal = Functions.trapezoidal(2, 4, 6, 8)
    val triangle = Functions.triangle(4, 5, 6)
    val gaussian = Functions.gaussian(5, 0.5)
  }

  trait TestUniverse {
    val testMap = HashMap(
      "Poor" -> Functions.gaussian(2, 0.5),
      "Good" -> Functions.gaussian(5, 0.5),
      "Excellent" -> Functions.gaussian(8, 0.5))

    val test = new Universe("Test", 0, 10, testMap)
  }

  test("universe apply returns correct value") {
    new TestUniverse {
      assert(test("Poor")(HashMap("Test" -> 5)) == test.getFunction("Poor")(5))
      assert(test("Good")(HashMap("Test" -> 5)) == test.getFunction("Good")(5))
      assert(test("Excellent")(HashMap("Test" -> 5)) == test.getFunction("Excellent")(5))
    }
  }

  test("numerical integration on polynomial is correct") {
    val f = (x: Double) => pow(x, 2) + 5 * x + 32
    val g = (x: Double) => pow(x, 3) / 3 + 5 * pow(x, 2) / 2 + 32 * x
    NumericalIntegration.integrate(f, 0, 10) shouldBe (g(10) - g(0) +- 0.001)
  }

  test("numerical integration on triangle is correct") {
    val f = Functions.triangle(4, 5, 6)
    NumericalIntegration.integrate(f, 0, 10) shouldBe (0.5 * 2 +- +.001)
  }

  test("centroid on triangle is correct") {
    val f = Functions.triangle(4, 5, 6)
    NumericalIntegration.centroid(f, 0, 10) shouldBe (5.0 +- +.001)
  }

  test("Centroid is found correctly for identity inference system") {
    val testInputMap = HashMap(
      "Poor" -> Functions.trapezoidal(0, 0, 3, 3),
      "Good" -> Functions.trapezoidal(3, 3, 7, 7),
      "Excellent" -> Functions.trapezoidal(7, 7, 10, 10))
    val testInput = new Universe("TestIn", 0, 10, testInputMap)

    val testOutputMap = HashMap(
      "Low" -> Functions.trapezoidal(0, 0, 3, 3),
      "Medium" -> Functions.trapezoidal(3, 3, 7, 7),
      "High" -> Functions.trapezoidal(7, 7, 10, 10))
    val testOutput = new Universe("TestOut", 0, 10, testOutputMap)

    val rule1 = new Rule(testInput("Poor"), testOutput.getFunction("Low"))
    val rule2 = new Rule(testInput("Good"), testOutput.getFunction("Medium"))
    val rule3 = new Rule(testInput("Excellent"), testOutput.getFunction("High"))

    val mamdani = new Mamdani(Set(rule1, rule2, rule3), testOutput)

    mamdani(HashMap("TestIn" -> 1.5)) shouldBe (1.5 +- +.01)
    mamdani(HashMap("TestIn" -> 5)) shouldBe (5.0 +- +.01)
    mamdani(HashMap("TestIn" -> 8)) shouldBe (8.5 +- +.01)
  }

  test("Centroid is found correctly for inference system with two partially overlapping triangles") {
    val testInputMap = HashMap(
      "Bad" -> Functions.trapezoidal(0, 0, 6, 6),
      "Good" -> Functions.trapezoidal(4, 4, 10, 10))
    val testInput = new Universe("TestIn", 0, 10, testInputMap)

    val testOutputMap = HashMap(
      "Low" -> Functions.triangle(0, 3, 6),
      "High" -> Functions.triangle(4, 7, 10))
    val testOutput = new Universe("TestOut", 0, 10, testOutputMap)

    val rule1 = new Rule(testInput("Bad"), testOutput.getFunction("Low"))
    val rule2 = new Rule(testInput("Good"), testOutput.getFunction("High"))

    val mamdani = new Mamdani(Set(rule1, rule2), testOutput)

    mamdani(HashMap("TestIn" -> 1)) shouldBe (3.0 +- +.01)
    mamdani(HashMap("TestIn" -> 5)) shouldBe (5.0 +- +.01)
    mamdani(HashMap("TestIn" -> 8)) shouldBe (7.0 +- +.01)
  }

  test("Centroid is found correctly for restaurant inference system") {
    val serviceMap = HashMap(
      "Poor" -> Functions.trapezoidal(0, 0, 0, 4),
      "Good" -> Functions.trapezoidal(1, 4, 6, 9),
      "Excellent" -> Functions.trapezoidal(6, 9, 10, 10))

    val service = new Universe("Service", 0, 10, serviceMap)

    val foodMap = HashMap(
      "Rancid" -> Functions.trapezoidal(0, 1, 1, 3),
      "Delicious" -> Functions.trapezoidal(7, 9, 10, 10))

    val food = new Universe("Food", 0, 10, foodMap)

    val tipMap = HashMap(
      "Cheap" -> Functions.triangle(0, 5, 10),
      "Average" -> Functions.triangle(10, 15, 20),
      "Generous" -> Functions.triangle(20, 25, 30))
    val tip = new Universe("Tip", 0, 30, tipMap)

    val rule1 = new Rule(service("Poor") and food("Rancid"), tip.getFunction("Cheap"))
    val rule2 = new Rule(service("Good"), tip.getFunction("Average"))
    val rule3 = new Rule(service("Excellent") and food("Delicious"), tip.getFunction("Generous"))

    val mamdani = new Mamdani(Set(rule1, rule2, rule3), tip)

    mamdani(HashMap("Food" -> 1, "Service" -> 1)) shouldBe (5.0 +- +.01)
    mamdani(HashMap("Food" -> 5, "Service" -> 5)) shouldBe (15.0 +- +.01)
    mamdani(HashMap("Food" -> 10, "Service" -> 10)) shouldBe (25.0 +- +.01)
  }
}
