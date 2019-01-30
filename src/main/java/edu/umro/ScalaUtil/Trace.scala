package edu.umro.ScalaUtil

import java.util.Date
import java.text.SimpleDateFormat

/**
 * For debugging code
 */
object Trace {

  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

  private def current: String = {
    val se = Thread.currentThread.getStackTrace()(3)
    val line = se.getLineNumber
    val method = se.getMethodName
    val threadName = Thread.currentThread.getName
    "Trace " + dateFormat.format(new Date) + " | " + threadName + " | " + se.toString
  }

  /** Print current line. */
  def trace: Unit = {
    println(current)
  }

  /** Print current line and parameter value. */
  def trace(v: Any): Unit = {
    val text = if (v == null) "null" else v.toString
    println(current + " : " + text)
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    trace("hey")
    trace
  }

}