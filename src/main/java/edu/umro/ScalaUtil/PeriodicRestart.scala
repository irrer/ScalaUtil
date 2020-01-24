package edu.umro.ScalaUtil

import java.util.GregorianCalendar
import java.util.Calendar
import java.util.Date
import java.text.SimpleDateFormat
import java.text.ParseException

/**
 * Shut down the current program so that it will automatically restart.
 *
 *
 * @author Jim Irrer
 */
class PeriodicRestart(restartTime: Long) extends Runnable with Logging {

  def this(restartTimeText: String) = this(PeriodicRestart.timeOfDayToMs(restartTimeText))

  /**
   * Wait for the given number of milliseconds, restarting the clock if there
   * is an <code>InterruptedException</code>.
   *
   * @param ms Number of millisecond to wait.
   */
  private def sleep(ms: Long) = {

    def now = System.currentTimeMillis
    val finish = now + ms;
    def remaining = finish - System.currentTimeMillis;

    while (finish > now) {
      try {
        Thread.sleep(remaining);
      } catch {
        case e: InterruptedException => logger.info("Unexpected exception while sleeping in PeriodicRestart: " + fmtEx(e))
      }
    }
  }

  private def getIntervalDescription(interval: Long): String = {
    val hour = interval / (1000 * 60 * 60)
    val minute = ((interval / (1000 * 60)) % 60).formatted(":%02d")
    val second = ((interval / 1000) % 60).formatted(":%02d")
    hour + minute + second
  }

  /**
   * Determine how long the service has to wait until the next restart time.
   *
   * @return Time in milliseconds until next restart time.
   */
  def waitTime: Long = {
    val oneDay: Long = 24 * 60 * 60 * 1000 // The length of one day in milliseconds
    val now = new GregorianCalendar

    val restart = new GregorianCalendar
    restart.setTimeInMillis(restartTime)

    val stop = new GregorianCalendar
    stop.setTimeInMillis(now.getTimeInMillis)
    stop.set(Calendar.HOUR_OF_DAY, restart.get(Calendar.HOUR_OF_DAY))
    stop.set(Calendar.MINUTE, restart.get(Calendar.MINUTE))
    stop.set(Calendar.SECOND, 0)
    stop.set(Calendar.MILLISECOND, 0)

    val interval = (stop.getTimeInMillis - now.getTimeInMillis + oneDay) % oneDay

    val stopDesc = new Date(if (stop.getTimeInMillis <= now.getTimeInMillis) stop.getTimeInMillis + oneDay else stop.getTimeInMillis).toString
    logger.info("Service restart time" + stopDesc + "    Wait time before restarting: " + getIntervalDescription(interval))
    interval
  }

  /**
   * Wait until the restart time specified in the configuration file, and then
   * terminate this service.  The YAJSW framework will restart a service when
   * it terminates with a non-zero status.
   */
  def run: Unit = {
    sleep(waitTime)
    logger.info("Intentionally terminating service to initiate YAJSW restart.")
    System.exit(1)
  }

  logger.info("Starting periodic restart thread.")
  (new Thread(this)).start
  logger.info("Periodic restart thread is running.")
}

object PeriodicRestart extends Logging {

  /** If the timeOfDayText to <code>timeOfDayToMs</code> is badly formatted, then default to this. */
  val DEFAULT_RESTART_TIME_TEXT = "2:22"

  /**
   * Convert time of day to ms.
   *
   * @param timeOfDayText: Time of day formatted as HH:MM or H:MM, the hour:minute (24 hour clock) to
   * restart.  If badly formatted, then the default is returned.
   *
   * @return Milliseconds past midnight.
   */
  def timeOfDayToMs(timeOfDayText: String): Long = {
    val dateFormat = new SimpleDateFormat("HH:mm")
    val millisec = try {
      dateFormat.parse(timeOfDayText).getTime
    } catch {
      case e: ParseException => {
        logger.warn("Badly formatted time of day text: " + timeOfDayText + " .  Should be HH:MM, as in 1:23 .  Assuming default of " + DEFAULT_RESTART_TIME_TEXT)
        dateFormat.parse(DEFAULT_RESTART_TIME_TEXT).getTime
      }
    }

    millisec
  }

  /**
   * For testing only.
   */
  def main(args: Array[String]): Unit = {

    println("current time: " + (new Date))
    val timeText = (new SimpleDateFormat("HH:mm")).format(new Date(System.currentTimeMillis + 60 * 1000))
    println("stop time: " + timeText)

    new PeriodicRestart(timeText)

    (0 until 64).map { s =>
      {
        Thread.sleep(1000)
        print("  " + s)
      }
    }
    println("\nfailed to properly exit")
    System.exit(0)
  }
}