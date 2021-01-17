package com.unihogsoft.rurki

import com.unihogsoft.rurki
import org.apache.commons.math3.analysis.integration.IterativeLegendreGaussIntegrator
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, DecompositionSolver, LUDecomposition}

object FEMCalculator {

  trait Expression[Arg] {
    def calculate(arg: Arg): Double
  }

  case class Sum[Arg](elements: Expression[Arg]*) extends Expression[Arg] {
    override def calculate(arg: Arg): Double =
      elements.map(_.calculate(arg)).sum
  }

  case class ArgFn[Arg](fn: Arg => Double) extends Expression[Arg] {
    override def calculate(arg: Arg): Double = fn(arg)
  }


  case class Integrate[Arg](fn: (Arg, Double) => Double, from: Double, to: Double) extends Expression[Arg] {
    override def calculate(arg: Arg): Double = {
      val integrator = new IterativeLegendreGaussIntegrator(16, 1e-5, 1e-5 )
      integrator.integrate(Integer.MAX_VALUE, x => fn(arg, x), from, to)
    }
  }

  private def baseFn(n: Int, from: Double, to: Double): Base = (i: Int) => {
    val step = (to - from ) / n
    val start = from + step * (i - 1)
    val mid = start + step
    val end = start + 2 * step
    val steepness = 1.0 / step

    val fn: Double => (Double, Double) = {
      case x if x >= start && x < mid => ((x - start) / step, steepness)
      case x if x >= mid && x < end => ((end - x) / step, -steepness)
      case _ => (0, 0)
    }

    FnWithDv(x => fn(x)._1, x => fn(x)._2)
  }

  def E(x: Double): Double = if(x < 1) 3 else 5
  private val bEquation = ∑(
    ∫((u, v, x) => E(x) * u.dv(x) * v.dv(x), 0, 2),
    λ((u, v) => -3 * u(0) * v(0))
  )

  private val lEquation = λ(v => -30*v(0))

  private def composeFunction(
    n: Int,
    start: Double,
    end: Double,
    base: Base,
    coeff: Array[Double],
    points: Int
  ): FnPlotWithBases = {
    val baseFns = (0 to n).map(base).map(_.fn)
    val weightedFns = baseFns.zip(coeff).map{
      case (f: Fn, w: Double) => (x: Double) => w * f(x)
    }
    val fn = weightedFns.reduce[Fn] {
      case (f1, f2) => (x: Double) => f1(x) + f2(x)
    }
    val step = (end - start) / points
    val fnPlot = (for {
      i <- 0 to points
      x = start + step * i
      y = fn(x)
    } yield x -> y).toList
    val basesPlot = (for {
      i <- (0 until n)
      step = (end - start) / n
      x1 = start + step * (i - 1)
      x2 = x1 + step
      x3 = x1 + 2 * step
    } yield List(x1,x2,x3).map(x => x -> weightedFns(i)(x))).toList
    (fnPlot, basesPlot)
  }

  def calculate(n: Int): FnPlotWithBases = {
    val start = 0
    val end = 2
    val base = baseFn(n, start, end)
    val coefficients = Array.tabulate(n, n)((x, y) => {
      println(s"Calculating for ${x} $y")
      bEquation.calculate(base(x), base(y))
    })
    val matrix = new Array2DRowRealMatrix(coefficients)
    val constants = new ArrayRealVector(Array.tabulate(n)(i => lEquation.calculate(base(i))))
    val solver = new LUDecomposition(matrix).getSolver
    val result = solver.solve(constants).toArray
    composeFunction(n, start, end, base, result, 1024)
  }
}
