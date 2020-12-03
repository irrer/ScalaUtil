package edu.umro.ScalaUtil

import java.util.Date
import java.text.SimpleDateFormat

/**
 * For debugging code.  Print source line and optoinally parameters.
 */
object Trace {

  private val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

  /** Time of last trace message. */
  private var time = System.currentTimeMillis

  /** If true, do tracing.  If false, do not trace.  Default is true. */
  private var active = true

  /**
   * Turn tracing on.
   */
  def on: Unit = {
    active = true
  }

  /**
   * Turn tracing off.
   */
  def off: Unit = {
    active = false
  }

  /**
   * Determine if tracing is on.
   *
   * @return True if tracing is on.
   */
  def isOn: Boolean = active

  private def current: String = {
    val se = Thread.currentThread.getStackTrace()(3)
    val line = se.getLineNumber
    val method = se.getMethodName
    val threadName = Thread.currentThread.getName
    val now = System.currentTimeMillis
    val elapsedText = {
      val e = (now - time) / 1000.0
      e.formatted("%6.3f")
    }
    time = now
    // sometimes the thread is useful, but mostly it is just noise
    //"Trace " + dateFormat.format(new Date(now)) + " | " + threadName + " | " + elapsedText + " | " + se.toString
    "Trace " + dateFormat.format(new Date(now)) + " | " + elapsedText + " | " + se.toString
  }

  /** Print current line with time elapsed since last trace. */
  def trace: Unit = {
    if (active)
      println(current)
  }

  /** Print current line and parameter value with time elapsed since last trace. */
  def trace(v: Any): Unit = {
    if (active) {
      val text = if (v == null) "null" else v.toString.replaceAll("\0", " ")
      println(current + " : " + text)
    }
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    trace("hey E")
    Thread.sleep(100)
    trace
  }

}