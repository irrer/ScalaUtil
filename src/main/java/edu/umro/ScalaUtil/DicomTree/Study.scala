package edu.umro.ScalaUtil.DicomTree

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date


case class Study(StudyInstanceUID: String) {
  private val seriesList = scala.collection.mutable.Map[String, Series]()

  private var studyDescription: Option[String] = None

  def getDescription: Option[String] = studyDescription

  /**
   * If there is a study description, then save it.
   *
   * @param al Look here for description.
   */
  private def setDescription(al: AttributeList): Unit = {
    if (studyDescription.isEmpty) {
      val at = al.get(TagFromName.StudyDescription)
      if (at != null) {
        val text = at.getSingleStringValueOrNull
        if ((text != null) && text.trim.nonEmpty) studyDescription = Some(text)
      }
    }
  }

  /**
   * Add a file by putting the related information into the data structures.
   *
   * @param file File to add
   * @param al   Attribute list reflecting contents of file.
   */
  def add(file: File, al: AttributeList): Unit = {
    setDescription(al)
    val SeriesInstanceUID = TreeUtil.getAttr(al, TagFromName.SeriesInstanceUID)
    if (!seriesList.contains(SeriesInstanceUID)) seriesList.put(SeriesInstanceUID, Series(SeriesInstanceUID, TreeUtil.getAttr(al, TagFromName.Modality)))
    seriesList(SeriesInstanceUID).add(file, al)
  }


  /**
   * Get date of study, which is the minimum date of all of the seres in the study.
   *
   * @return Earliest date.
   */
  def dateOf: Date = seriesList.values.map(s => s.dateOf()).minBy(d => d.getTime)


  /**
   * Move series in this study.
   *
   * @param parentDir Put files under this directory.
   */
  def move(parentDir: File, parentDateTimeFormat: SimpleDateFormat): Unit = {
    def dirNameOf(series: Series): String = {
      parentDateTimeFormat.format(series.dateOf()) + "_" + series.Modality
    }

    val dateFormat = TreeUtil.dateTimeFormat(seriesList.values.map(s => s.dateOf()))
    seriesList.values.foreach(s => s.move(parentDir, dateFormat))

    /*
    if (seriesList.size > 1) {
    }
    else {
      seriesList.values.head.move(parentDir, parentDateTimeFormat)
    }
      */
  }

}

