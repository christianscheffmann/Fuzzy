package fuzzy

class Mamdani(rules: Set[Rule], output: Universe) {
  def apply(parameters: Map[String, Double]): Double = {
    val consequentAgg = (s: Set[Rule]) => s.foldLeft(new Rule(_ => _ => 0.0))((a, b) => a.agg(b))
    NumericalIntegration.centroid(consequentAgg(rules)(parameters), output.lowerLimit, output.upperLimit)
  }
}