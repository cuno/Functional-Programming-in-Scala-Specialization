package calculator

object Polynomial {

  import scala.math.sqrt

  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = Signal {
    val _4ac = 4 * a() * c()
    val _b = b()
    _b * _b - _4ac
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val (_a_, _b_) = (a(), b())
      val _2a = 2 * _a_
      delta() match {
        case _Δ_ if _Δ_ < 0 =>
          Set.empty
        case _Δ_ if _Δ_ > 0 =>
          val sd = sqrt(_Δ_)
          Set(
            (-_b_ + sd) / _2a, (-_b_ - sd) / _2a)
        case _ =>
          Set(-_b_ / _2a)
      }
    }
  }

}
