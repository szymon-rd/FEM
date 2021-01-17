package com.unihogsoft.rurki


import javafx.beans.value.{ChangeListener, ObservableValue}
import scalafx.application.{JFXApp, Platform}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.geometry.Pos.Center
import scalafx.scene.AccessibleRole.TextArea
import scalafx.scene.control.{Button, CheckBox, Label, ProgressBar, TextArea, TextField}
import scalafx.scene.layout.{BorderPane, HBox}
import FEMCalculator._

import scala.concurrent.{ExecutionContext, Future}
object Main extends JFXApp {

  implicit val ec = ExecutionContext.global

  // Defining the axes
  val xAxis = new NumberAxis()
  val yAxis = new NumberAxis()

  // Creating the chart
  val lineChart: LineChart[Number, Number] = LineChart(xAxis, yAxis)
  lineChart.title = "Finite Elements Method"
  lineChart.setCreateSymbols(false)
  lineChart.legendVisible = false

  def mapToSeries(plot: FnPlot) = {
    val data = ObservableBuffer(plot.map{case (x, y) => XYChart.Data[Number, Number](x, y)})
    XYChart.Series[Number, Number](data)
  }

  val pointsInput = new TextField {
    text = "12"
    margin = Insets(10)
  }

  pointsInput.text.addListener(ScalafxUtil.onlyIntegerGuard(pointsInput))

  def pointsValue = pointsInput.text.value.toInt

  val drawBase = new CheckBox("Draw bases") {
    selected = true
    margin = Insets(10)
  }

  val nInput = new TextField {
    text = "3"
    margin = Insets(10)
  }

  nInput.text.addListener(ScalafxUtil.onlyIntegerGuard(nInput))

  def nValue: Int = nInput.text.value.toInt

  val progressBar = new ProgressBar(){
    progress = 1
  }

  def calculate() = {
    if(nInput.text.isEmpty.get()) nInput.text = "3"
    lineChart.getData.clear()
    FEMCalculator(pointsValue).calculate(nValue, progressBar.setProgress).map {
      case (fn, bases) =>
        Platform.runLater {
          if(drawBase.selected.value) bases.map(b => lineChart.getData.add(mapToSeries(b)))
          lineChart.getData.add(mapToSeries(fn))
          progressBar.setProgress(1)
        }
    }
  }

  stage = new PrimaryStage {
    title = "FEM"
    scene = new Scene(1024, 600) {
      root = new BorderPane {
        center = lineChart
        bottom = new HBox {
          alignment = Center
          children = List(
            new HBox {
              children = List(
                new Label("n = "),
                nInput,
                new Label("points = "),
                pointsInput,
                drawBase
              )
              margin = Insets(10)
              alignment = Center
            },
            new Button("Calculate") {
              margin = Insets(10)
              onAction = _ => calculate()
            },
            progressBar
          )
        }
      }
    }
  }
}