package com.unihogsoft.rurki

import com.unihogsoft.rurki.Main.nInput
import javafx.beans.value.{ChangeListener, ObservableValue}
import scalafx.scene.control.TextField

object ScalafxUtil {

  def onlyIntegerGuard(field: TextField): ChangeListener[String] = (_, oldValue: String, newValue: String) => {
    if (!newValue.matches("\\d*")) field.text = oldValue
  }
}
