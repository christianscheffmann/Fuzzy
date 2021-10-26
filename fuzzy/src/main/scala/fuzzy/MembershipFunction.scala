package fuzzy

import scala.math.{max, min}

class MembershipFunction(m: Map[String, Double] => Double) {
  def apply(parameters: Map[String, Double]): Double = m(parameters)

  def or(that: MembershipFunction): MembershipFunction = new MembershipFunction((parameters: Map[String, Double]) => max(this (parameters), that(parameters)))

  def and(that: MembershipFunction): MembershipFunction = new MembershipFunction((parameters: Map[String, Double]) => min(this (parameters), that(parameters)))

  def not(): MembershipFunction = new MembershipFunction((parameters: Map[String, Double]) => 1 - this (parameters))
}