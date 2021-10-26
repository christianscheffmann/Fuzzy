package fuzzy

import scala.collection.immutable.HashMap

object Main extends App {

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

  println(mamdani(HashMap("Food" -> 0.2, "Service" -> 0)))
}













