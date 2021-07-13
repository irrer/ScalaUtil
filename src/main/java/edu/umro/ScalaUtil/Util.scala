package edu.umro.ScalaUtil

import java.io.File
import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Properties
import java.util.UUID
import scala.annotation.tailrec
import scala.xml.Elem
import scala.xml.PrettyPrinter

/**
 * Common utilities for API.
 */
object Util {

  def makeUID: String = {
    UUID.randomUUID.toString
  }

  /**
   * System independent line separator.
   */
  val LS: String = System.getProperty("line.separator")

  def xmlToText(document: Elem): String = new PrettyPrinter(1024, 2).format(document)

  /**
   * Format a <code>Throwable</code>.
   *
   * @param throwable Contains description and stack trace.
   * @return Human readable version of <code>Throwable</code> and stack trace.
   */
  def fmtEx(throwable: Throwable): String = {
    throwable.getStackTrace.toList.foldLeft("")((_, stkElem) => "\n    " + stkElem)
  }

  // To be used as a lock for date formatting and parsing.
  private val dateSync = ""


  /**
   * Format a date in a thread safe way.  Note that the entire application has to use this or threads could
   * interfere with each other.  This is just a thread-safe wrapper for the standard function.
   *
   * @param format Specifies format.
   * @param date   The date to format.
   * @return Date formatted as string.
   */
  def formatDate(format: SimpleDateFormat, date: Date): String = dateSync.synchronized {
    format.format(date)
  }

  /**
   * Parse a date in a thread safe way.  Note that the entire application has to use this or threads could
   * interfere with each other.  This is just a thread-safe wrapper for the standard function.
   *
   * @param format Specifies format.
   * @param text   The formatted date to format.
   * @return Date formatted as string.
   */
  @throws[ParseException]
  def parseDate(format: SimpleDateFormat, text: String): Date = dateSync.synchronized {
    format.parse(text)
  }


  //noinspection SpellCheckingInspection
  private val standardDateFormatList = List(
    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX"),
    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX"),
    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS"),
    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss"),
    new SimpleDateFormat("yyyy-MM-dd' 'HH:mm:ss"))

  def dateToText(date: Date): String = standardDateFormatList.head.format(date)

  def standardFormat(date: Date): String = dateToText(date)

  def textToDate(text: String): Date = {
    val maxTextLength = 23
    val t = if (text.length > maxTextLength) text.substring(0, maxTextLength) else text

    def parse(dateFormat: SimpleDateFormat): Either[String, Date] = {
      try {
        Right(dateFormat.parse(t))
      } catch {
        case e: ParseException => Left("Attempted to use date format " + dateFormat.toPattern + " but failed with " + e.getMessage)
      }
    }

    val dateList = standardDateFormatList.map(fmt => parse(fmt))
    val validList = dateList.filter(d => d.isRight)
    if (validList.isEmpty) throw new ParseException("Unable to parse '" + text + "' as a date string", 0)
    val x = validList.head.right.get
    x
  }

  private val justDate = new SimpleDateFormat("yyyy-MM-dd")

  /**
   * Round off the given date+time to just date, e.g. Jan 24 1956 15:24:56 --> Jan 24 1956 00:00:00
   */
  def roundToDate(date: Date): Date = justDate.parse(justDate.format(date))

  private val elapsedFormatHours = new SimpleDateFormat("HH:mm:ss")
  private val elapsedFormatMinutes = new SimpleDateFormat("m:ss.SSS")
  private val elapsedFormatSeconds = new SimpleDateFormat("s.SSS")

  /**
   * Format time interval into a user friendly format.  Show more or less precision depending on
   * the size of <code>time</code>.
   *
   * @param interval Interval in ms.
   * @return Formatted as text.
   */
  def intervalTimeUserFriendly(interval: Long): String = {
    val intervalMs = interval.abs
    val fmt =
      if (intervalMs >= (60 * 60 * 1000))
        elapsedFormatHours
      else if (intervalMs >= (60 * 1000))
        elapsedFormatMinutes
      else
        elapsedFormatSeconds
    val text = fmt.format(new Date(intervalMs))
    // add negative symbol if interval is less than zero
    if (intervalMs < 0)
      "-" + text
    else
      text
  }


  /**
   * Given a list, group them into smaller groups of a given size.  The last group in the list may be smaller than the others.
   */
  def sizedGroups[T](seq: Seq[T], groupSize: Int): Seq[Seq[T]] = {
    val gs = Math.max(groupSize, 1)

    @tailrec
    def addGroup[TT](seq: Seq[TT], grp: Seq[Seq[TT]]): Seq[Seq[TT]] = {
      if (seq.isEmpty) grp
      else addGroup(seq.drop(gs), grp :+ seq.take(gs))
    }

    addGroup(seq, Seq[Seq[T]]())
  }

  def getJarFile(any: Any): Option[File] = {
    try {
      val clazz = any.getClass
      val classLoader = clazz.getClassLoader
      val pkg = clazz.getPackage
      val packageName = pkg.getName.replace('.', '/')
      val url = classLoader.getResource(packageName)
      val file = new File(url.getFile)
      Some(file)
    } catch {
      case _: Throwable => None
    }
  }

  /**
   * Geh the properties from the file in the given path in the jar of the given class.
   *
   * @param classy Class that is in the related jar file.
   *               *
   * @param path   Path within the jar file where the property file resides.
   *               *
   * @return Either a set of properties or, if anything went wrong, nothing.
   */
  def getJarPropertyFile(classy: java.lang.Class[_], path: String = "/manifest.properties"): Option[Properties] = {
    try {
      val p = new Properties
      val i = classy.getResourceAsStream(path)
      p.load(i)
      Some(p)
    } catch {
      case _: Exception => None
    }
  }

  /** For testing only. */
  def main(args: Array[String]): Unit = {
    println("Properties: " + getJarPropertyFile(LS.getClass))

    println("uuid: " + makeUID)


    class Foo {}
    val foo = new Foo

    getJarFile(foo)
    val s = {
      <hey>hey</hey>
    }
    getJarFile(s)
  }

}