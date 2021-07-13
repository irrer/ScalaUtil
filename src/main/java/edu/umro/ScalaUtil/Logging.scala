package edu.umro.ScalaUtil

import org.slf4j.Logger

/**
 * Log messages.  Extend this trait and use the <code>logger</code> value to log messages.
 *
 * To bind slf4j to log4j, the following VM properties should be set, as in:
 *
 * <pre>
 *
 *     -Djava.util.logging.manager=org.apache.logging.log4j.jul.LogManager
 *     -Dlog4j2.configurationFile=src\main\resources\log4j2.xml
 *
 * </pre>
 *
 *
 *
 * Examples of use:
 *
 * <h3>Scala</h3>
 * <pre>
 *
 *     class MyClass extends Logging {
 *         logger.info("Hello from MyClass")
 *
 *         class SubClass {
 *             logger.trace("Hello from SubClass")
 *         }
 *     }
 *
 *     object MyObject extends Logging {
 *         logger.warn("Scary hello from MyObject")
 *     }
 *
 * </pre>
 */
trait Logging {
  protected val logger: Logger = org.slf4j.LoggerFactory.getLogger(this.getClass)

  /**
   * Format a <code>Throwable</code>.
   *
   * @param throwable Contains description and stack trace.
   *
   * @return Human readable version of <code>Throwable</code> and stack trace.
   */
  def fmtEx(throwable: Throwable): String = {
    val textList = throwable.getStackTrace.map(ste => "\n    " + ste) // convert to text
    textList.foldLeft(throwable.toString)((t, ste) => t + ste) // join as one string
  }
}