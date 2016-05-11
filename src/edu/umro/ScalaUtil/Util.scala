package edu.umro.ScalaUtil

import scala.xml.PrettyPrinter
import scala.xml.Elem
import edu.umro.util.UMROGUID
import java.util.UUID
import java.util.Date
import java.text.SimpleDateFormat
import java.text.ParseException

/**
 * Common utilities for API.
 */
object Util {

    def makeUID: String = {
        UUID.randomUUID.toString
    }

    def xmlToText(document: Elem): String = new PrettyPrinter(1024, 2).format(document)

        /**
     * Format a <code>Throwable</code>.
     *
     * @param throwable Contains description and stack trace.
     *
     * @return Human readable version of <code>Throwable</code> and stack trace.
     */
    def fmtEx(throwable: Throwable): String = {
        throwable.getStackTrace.toList.foldLeft("")((text, stkElem) => "\n    " + stkElem)
    }
    
    private val standardDateFormatList = List(
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS"),
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss"),
        new SimpleDateFormat("yyyy-MM-dd' 'HH:mm:ss"))

    def dateToText(date: Date): String = standardDateFormatList(0).format(date)

    def textToDate(text: String): Date = {
        val maxTextLength = 23
        val t = if (text.size > 23) text.substring(0, 23) else text
        def parse(dateFormat: SimpleDateFormat): Either[String, Date] = {
            try {
                Right(dateFormat.parse(t))
            }
            catch {
                case e: ParseException => Left("Attempted to use date format " + dateFormat.toPattern + " but failed with " + e.getMessage())
            }
        }

        val dateList = standardDateFormatList.map(fmt => parse(fmt))
        val validList = dateList.filter(d => d.isRight)
        if (validList.isEmpty) throw new ParseException("Unable to parse '" + text + "' as a date string", 0)
        val x = validList(0).right.get
        x
    }
    
        /** For testing only. */
    def main(args: Array[String]): Unit = {
        println("uuid: " + makeUID)
    }

}