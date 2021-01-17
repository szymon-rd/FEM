package com.unihogsoft.rurki


import javafx.beans.value.{ChangeListener, ObservableValue}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.geometry.Pos.Center
import scalafx.scene.AccessibleRole.TextArea
import scalafx.scene.control.{Button, CheckBox, Label, TextArea, TextField}
import scalafx.scene.layout.{BorderPane, HBox}
object Main extends JFXApp {

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

  val nInput = new TextField {
    text = "3"
  }

  val drawBase = new CheckBox("Draw bases") {
    selected = true
    margin = Insets(10)
  }

  nInput.text.addListener(new ChangeListener[String] {
    override def changed(observableValue: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
      if(!newValue.matches("\\d*")) nInput.text = oldValue
    }
  })

  def nValue: Int = nInput.text.value.toInt

  def calculate() = {
    if(nInput.text.isEmpty.get()) nInput.text = "3"
    lineChart.getData.clear()
    val (fn, bases) = FEMCalculator.calculate(nValue)
    if(drawBase.selected.value) bases.map(b => lineChart.getData.add(mapToSeries(b)))
    lineChart.getData.add(mapToSeries(fn))
  }

  stage = new PrimaryStage {
    title = "FEM"
    scene = new Scene(800, 600) {
      root = new BorderPane {
        center = lineChart
        bottom = new HBox {
          alignment = Center
          children = List(
            new HBox {
              children = List(
                new Label("n = "),
                nInput,
                drawBase
              )
              margin = Insets(10)
              alignment = Center
            },
            new Button("Calculate") {
              margin = Insets(10)
              onAction = _ => calculate()
            }
          )
        }
      }
    }
  }
}