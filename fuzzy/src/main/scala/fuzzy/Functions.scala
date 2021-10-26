package fuzzy

import scala.math.{exp, pow}

object Functions {
  def trapezoidal(start: Double, topStart: Double, topStop: Double, stop: Double): Double => Double = (x: Double) => {
    if (x <= start) 0
    else if (x > start && x < topStart) (x - start) / (topStart - start)
    else if (x >= topStart && x <= topStop) 1
    else if (x > topStop && x < stop) (stop - x) / (stop - topStop)
    else 0
  }

  def triangle(start: Double, center: Double, stop: Double): Double => Double = trapezoidal(start, center, center, stop)

  def gaussian(center: Double, width: Double): Double => Double = (x: Double) => {
    exp(-1 / 2 * pow(x - center, 2) / pow(width, 2))
  }
}