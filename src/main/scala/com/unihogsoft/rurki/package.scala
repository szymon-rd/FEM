package com.unihogsoft

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.{ExecutionContext, Future}

package object rurki {
  implicit val ec = ExecutionContext.global
  def timed[T](f: => Future[T]): Future[T] = {
    val startTime = System.currentTimeMillis()
    f.map { x =>
      val delta = System.currentTimeMillis() - startTime
      println(s"It took: $delta ms")
      x
    }
  }
}
