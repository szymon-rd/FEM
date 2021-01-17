package com.unihogsoft

import com.unihogsoft.rurki.FEMCalculator.{ArgFn, Expression, Integrate, Sum}

package object rurki {
  type UV = (FnWithDv, FnWithDv)
  type V = FnWithDv
  type Base = Int => FnWithDv

  type FnPlot = List[(Double, Double)]
  type FnPlotWithBases = (FnPlot, List[FnPlot])
  type Fn = Double => Double
  case class FnWithDv(
    fn: Fn, dv: Fn
  ) {
    def apply(x: Double): Double = fn(x)
  }

  def ∫(fn: (FnWithDv, FnWithDv, Double) => Double, from: Double, to: Double): Integrate[UV] =
    Integrate((arg, x) => fn(arg._1, arg._2, x), from, to)
  def ∑[T](xs: Expression[T]*): Sum[T] = Sum.apply(xs: _*)
  def λ(fn: (FnWithDv, FnWithDv) => Double): ArgFn[UV] = ArgFn(fn.tupled)
  def λ(fn: (FnWithDv) => Double): ArgFn[V] = ArgFn(fn)
}
