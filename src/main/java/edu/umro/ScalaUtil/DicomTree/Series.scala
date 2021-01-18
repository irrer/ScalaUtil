package edu.umro.ScalaUtil.DicomTree

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date


private case class Series(SeriesInstanceUID: String, Modality: String) {
  private val dicomFileList = scala.collection.mutable.Map[String, DicomFile]()

  private var seriesDescription: Option[String] = None

  def getDescription: Option[String] = seriesDescription

  def size(): Int = dicomFileList.size

  def forList(): Seq[Int] = dicomFileList.values.toIndexedSeq.flatMap(df => df.forInInst).distinct.sorted

  /**
   * If there is a series description, then save it.
   *
   * @param al Look here for description.
   */
  private def setDescription(al: AttributeList): Unit = {
    if (seriesDescription.isEmpty) {
      val at = al.get(TagFromName.SeriesDescription)
      if (at != null) {
        val text = at.getSingleStringValueOrNull
        if ((text != null) && text.trim.nonEmpty) seriesDescription = Some(text)
      }
    }
  }


  /**
   * Add a file by putting the related information into the data structures.
   *
   * @param sourceFile File to add
   * @param al         Attribute list reflecting contents of file.
   */
  def add(sourceFile: File, al: AttributeList): Unit = {
    val SOPInstanceUID = TreeUtil.getAttr(al, TagFromName.SOPInstanceUID)
    if (dicomFileList.contains(SOPInstanceUID)) {
      println("\nFile with duplicate SOPInstanceUID ignored: " + sourceFile.getAbsolutePath + "  Previous file: " + dicomFileList(SOPInstanceUID).sourceFile.getAbsolutePath)
    }
    else dicomFileList.put(SOPInstanceUID, DicomFile.constructDicomFile(al, sourceFile))
  }


  /**
   * Get date of series, which is the minimum date of all of the files in the series.
   *
   * @return Earliest date.
   */
  def dateOf(): Date = dicomFileList.values.minBy(df => df.date.getTime).date


  /**
   * Move the files in this series to the parent directory.
   *
   * If there are multiple DICOM files in the series, then create a series directory and put the files
   * there.  If there is just one DICOM file, then put it in the parent directory.
   *
   * @param parentDir        Put them under this directory.
   * @param parentDateFormat Use this date format for prefixing.
   */
  def move(parentDir: File, parentDateFormat: SimpleDateFormat): Unit = {

    if (dicomFileList.size == 1) {
      // There is only one file in this series, so do not create an extra directory
      // level.  Just put the DICOM file directly in the parent dir.

      val fileName =
        parentDateFormat.format(dateOf()) + "_" +
          Modality + "_" +
          TreeUtil.forToString(forList()) +
          DicomTree.dicomFileSuffix
      val file = new File(parentDir, fileName)
      dicomFileList.values.head.sourceFile.renameTo(file)
    }

    else {
      // There are multiple DICOM files in this series, so create a series directory and put the DICOM files in it.

      val indexFormat = "%0" + dicomFileList.size.toString.length + "d"

      val desc: String = {
        if (seriesDescription.isDefined) "_" + TreeUtil.formatDescription(seriesDescription.get, DicomTree.maxSeriesDescriptionSize)
        else ""
      }

      val seriesDir = new File(parentDir, parentDateFormat.format(dateOf()) + "_" + Modality + dicomFileList.size + desc)
      seriesDir.mkdirs()

      def moveFile(df: DicomFile, index: Int): Unit = {
        val fileName = Modality + "_" + (index + 1).formatted(indexFormat) + DicomTree.dicomFileSuffix
        val file = new File(seriesDir, fileName)
        df.sourceFile.renameTo(file)
      }

      val sortedList = dicomFileList.values.toIndexedSeq.sortBy(df => df.sortingText + TreeUtil.standardDateFormat.format(df.date))
      sortedList.zipWithIndex.foreach(dfIndex => moveFile(dfIndex._1, dfIndex._2))
    }
  }

}

