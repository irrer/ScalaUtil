package edu.umro.ScalaUtil.DicomSort

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.tailrec

object TreeUtil {

  /**
   * Given a list of FOR (frame of reference UID) indexes, format it as text with a leading _.  Example
   *
   * Seq(1,2,3,4) --> _FOR_1_FOR_2_FOR_3_FOR_4
   *
   * @param forList List of FOR indexes.
   * @return
   */
  def forToString(forList: Seq[Int]): String = {
    if (forList.isEmpty) ""
    else forList.map(i => "FOR" + (i + 1)).mkString("_", "_", "")
  }

  val standardDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss.SSS")

  /** Maximum number of characters to use for descriptions.  They can be hundreds of characters, which
   * makes unreasonably long file names.
   */


  /**
   * Get an attribute as a string.  It is important to create a copy of the string so that the
   * attribute list can be garbage collected.  This keeps the program's memory footprint small,
   * which is important when dealing with zillions of files.
   *
   * @param al  Get from this list
   * @param tag Tag to get
   * @return A copy of the attributes as text with whitespace trimmed.
   */
  def getAttr(al: AttributeList, tag: AttributeTag): String = {
    new String(al.get(tag).getSingleStringValueOrEmptyString.trim)
  }


  /**
   * Search for a usable date+time in an attribute list.  First valid one wins, so the order matters.
   */
  private val dateTimeTagPairs = Seq(
    (TagFromName.ContentDate, TagFromName.ContentTime),
    (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
    (TagFromName.SeriesDate, TagFromName.SeriesTime),
    (TagFromName.StudyDate, TagFromName.StudyTime),
    (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime),
    (TagFromName.TreatmentDate, TagFromName.TreatmentTime),
    (TagFromName.RTPlanDate, TagFromName.RTPlanTime),
    (TagFromName.StructureSetDate, TagFromName.StructureSetTime))

  /**
   * Try getting a date that represents when the file was created.
   *
   * @param al Get from this list
   * @return A date that represents when the file was created.
   */
  def getDateTime(al: AttributeList): Date = {
    val date = dateTimeTagPairs.flatMap(dtp => DicomUtil.getTimeAndDate(al, dtp._1, dtp._2)).head
    date
  }


  /**
   * Find a date-time format that will format to a different string for each date given while
   * also minimizing the amount of redundant information shown to the user.
   *
   * @param dateList List of dates that must be uniquely formatted.
   * @return
   */
  def dateTimeFormat(dateList: Iterable[Date]): SimpleDateFormat = {
    val date = new SimpleDateFormat("yyyy-MM-dd")
    val dateTime = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm")
    val dateTimeSec = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")
    val dateTimeSecMS = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss.SSS")

    /**
     * Determine if the given format will yield a different string for each date.
     *
     * @param format Format to try.
     * @return True if all distinct
     */
    def allDistinct(format: SimpleDateFormat): Boolean = dateList.map(d => format.format(d)).toIndexedSeq.distinct.size == dateList.size


    /**
     * Test each format in the list in the order given and stop when a format is found that will
     * yield different results for each date.
     *
     * @param formatList List of formats to consider.
     * @return
     */
    @tailrec
    def leastGranularOf(formatList: Seq[SimpleDateFormat]): SimpleDateFormat = {
      if (formatList.size == 1) formatList.head
      else if (allDistinct(formatList.head)) formatList.head
      else leastGranularOf(formatList.tail)
    }

    val format: SimpleDateFormat = leastGranularOf(Seq(date, dateTime, dateTimeSec, dateTimeSecMS))

    format
  }


  /**
   * Given a description, minimize it by
   *   - removing blanks
   *   - replacing characters that are not permitted in file names with '_'
   *   - replace multiple consecutive '_' with a single one
   *   - trim to maximum given size
   *   - remove leading and trailing '_'
   *
   * @param description Initial description.
   * @param maxSize     Maximum allowed size of description in characters.
   * @return A formatted version of the description.
   */
  def formatDescription(description: String, maxSize: Int): String = {
    val minimized: String = description.replaceAll(" ", "")
    val formatted = FileUtil.replaceInvalidFileNameCharacters(minimized, '_').
      replaceAll("__*", "_").
      take(maxSize).
      replaceAll("^_", "").
      replaceAll("_$", "")
    formatted
  }


  /**
   * Stop reading DICOM before the pixel data.
   */
  private class DicomReadStrategy extends ReadTerminationStrategy {
    override def terminate(attributeList: AttributeList, tag: AttributeTag, byteOffset: Long): Boolean = {
      tag.getGroup >= 0x6000
    }
  }

  private val dicomReadStrategy = new DicomReadStrategy

  /**
   * Read a DICOM file.  If it contains DICOM, then return the corresponding attribute list.
   *
   * @param dicomFile Try to read this as a DICOM file.
   * @return An attribute list or  nothing on failure.
   */
  def readFile(dicomFile: File): Option[AttributeList] = {
    try {
      val al = new AttributeList
      al.read(dicomFile, dicomReadStrategy)
      print(".") // show read progress to user
      Some(al)
    } catch {
      case _: Throwable =>
        println("Unable to read file as DICOM.  Ignoring: " + dicomFile.getAbsolutePath)
        None
    }
  }


  /**
   * Safely get a list of files in a directory.  On failure, return an empty list.
   */
  def listFilesSafely(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    } catch {
      case t: Throwable => List[File]()
    }
  }


  def renameFile(oldFile: File, newFile: File): Boolean = {
    if (newFile.exists()) false
    else oldFile.renameTo(newFile)
  }

}
