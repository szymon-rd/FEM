package com.unihogsoft.rurki

import java.lang.Math._
import java.util.concurrent.Executors
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import com.unihogsoft.rurki
import org.apache.commons.math3.analysis.integration.IterativeLegendreGaussIntegrator
import org.apache.commons.math3.linear.{Array2DRowRealMatrix, ArrayRealVector, DecompositionSolver, LUDecomposition}
import rurki._
import FEMCalculator._

import scala.concurrent.{ExecutionContext, Future}

case class FEMCalculator(integrationPoints: Int) {

  private implicit val integrator = ThreadLocal.withInitial(
    () => new IterativeLegendreGaussIntegrator(integrationPoints, 1e-5, 1e-5 )
  )
  implicit val integratorProvider: IntegratorProvider = () => integrator.get()
  private implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors() * 4))

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

    (FnWithDv(x => fn(x)._1, x => fn(x)._2), (start, end))
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
    val baseFns = (0 to n).map(base).map(_._1.fn)
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

  private def combineRange(a: Range, b: Range, limit: Range): Range = (a, b, limit) match {
    case ((aStart, aEnd), (bStart, bEnd), (limitStart, limitEnd)) =>
      (max(min(aStart, bStart), limitStart), min(max(aEnd, bEnd), limitEnd))
  }

  private def threadsafeProgressReporter(progressReporter: Double => Unit, totalOps: Int) = {
    val progress = new AtomicInteger()
    () => {
      val toReport = progress.incrementAndGet().toDouble / totalOps
      progressReporter(toReport)
    }
  }

  def calculate(n: Int, progressReporter: Double => Unit): Future[FnPlotWithBases] = {
    val start = 0
    val end = 2
    val limitRange = (start.toDouble, end.toDouble)
    val base = baseFn(n, start, end)
    val reporter = threadsafeProgressReporter(progressReporter, n * 3)
    val coefficientsFuture: List[List[Future[Double]]] = List.tabulate(n, n)((x, y) => Future {
      if(abs(x - y) <= 1) {
        val (baseX, rangeX) = base(x)
        val (baseY, rangeY) = base(y)
        val arg = (baseX, baseY)
        val range = combineRange(rangeX, rangeY, limitRange)
        val result = bEquation.calculate(arg, range)
        reporter()
        result
      } else 0
    })
    Future.sequence(coefficientsFuture.map(x => Future.sequence(x)))
      .map(_.map(_.toArray).toArray).map { coefficients =>
      val matrix = new Array2DRowRealMatrix(coefficients)
      val constants = new ArrayRealVector(Array.tabulate(n)(i => (lEquation.calculate _).tupled(base(i))))
      val solver = new LUDecomposition(matrix).getSolver
      val result = solver.solve(constants).toArray
      composeFunction(n, start, end, base, result, 1024)
    }
  }
}

object FEMCalculator {
  type UV = (FnWithDv, FnWithDv)
  type V = FnWithDv
  type Range = (Double, Double)
  type Base = Int => (FnWithDv, Range)

  type FnPlot = List[(Double, Double)]
  type FnPlotWithBases = (FnPlot, List[FnPlot])
  type Fn = Double => Double
  type IntegratorProvider = () => IterativeLegendreGaussIntegrator
  case class FnWithDv(
    fn: Fn, dv: Fn
  ) {
    def apply(x: Double): Double = fn(x)
  }

  def ∫(fn: (FnWithDv, FnWithDv, Double) => Double, from: Double, to: Double)(implicit integrator: IntegratorProvider): Integrate[UV] =
    Integrate((arg, x) => fn(arg._1, arg._2, x), from, to)
  def ∑[T](xs: Expression[T]*): Sum[T] = Sum.apply(xs: _*)
  def λ(fn: (FnWithDv, FnWithDv) => Double): ArgFn[UV] = ArgFn(fn.tupled)
  def λ(fn: (FnWithDv) => Double): ArgFn[V] = ArgFn(fn)

  trait Expression[Arg] {
    def calculate(arg: Arg, nonzeroRange: Range): Double
  }

  case class Sum[Arg](elements: Expression[Arg]*) extends Expression[Arg] {
    override def calculate(arg: Arg, nonzeroRange: Range): Double =
      elements.map(_.calculate(arg, nonzeroRange)).sum
  }

  case class ArgFn[Arg](fn: Arg => Double) extends Expression[Arg] {
    override def calculate(arg: Arg, nonzeroRange: Range): Double = fn(arg)
  }


  case class Integrate[Arg](fn: (Arg, Double) => Double, from: Double, to: Double)(implicit integrator: IntegratorProvider) extends Expression[Arg] {
    override def calculate(arg: Arg, nonzeroRange: Range): Double = {
      integrator().integrate(Integer.MAX_VALUE, x => fn(arg, x), nonzeroRange._1, nonzeroRange._2)
    }
  }
}