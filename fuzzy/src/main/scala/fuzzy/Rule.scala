package fuzzy

import scala.math.{max, min}

class Rule(rule: Map[String, Double] => Double => Double) {
  def this(antecedent: MembershipFunction, consequent: Double => Double) = this((parameters: Map[String, Double]) => (y: Double) => min(antecedent(parameters), consequent(y)))

  def apply(parameters: Map[String, Double])(y: Double): Double = rule(parameters)(y)

  def agg(that: Rule): Rule = new Rule((parameters: Map[String, Double]) => (y: Double) => max(this(parameters)(y), that(parameters)(y)))
}