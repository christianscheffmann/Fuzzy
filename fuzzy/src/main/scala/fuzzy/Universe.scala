package fuzzy

import scala.collection.immutable.HashMap

class Universe(n: String, ll: Double, ul: Double, m: Map[String, Double => Double]) {
  val name: String = n
  val lowerLimit: Double = ll
  val upperLimit: Double = ul
  val memberships: Map[String, MembershipFunction] = m.map(mm => (mm._1, new MembershipFunction((parameters: Map[String, Double]) => mm._2(parameters(name)))))

  def apply(setName: String): MembershipFunction = memberships(setName)

  def getFunction(setName: String): Double => Double = (y: Double) => this (setName)(HashMap(name -> y))
}