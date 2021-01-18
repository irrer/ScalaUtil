package edu.umro.ScalaUtil.DicomTree

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName

import java.io.File
import java.util.Date


/**
 * Represent a single DICOM file.
 *
 * While it would be simpler to include the attribute list in the class parameters, that would require too much
 * memory to store them all, so this is constructed with the <code>DicomFile.getSortingText</code> function
 * which allows the attribute lists to be garbage collected.
 *
 * @param forInInst  List of frame of reference indexes.
 * @param sourceFile Source file.
 * @param date       Date associated with file.
 */
case class DicomFile(forInInst: Seq[Int], sourceFile: File, date: Date, sortingText: String) {}

object DicomFile {

  /**
   * Get a string that when compared to others, with sort files in the way that the user wants.  The attributes
   * used reflect physical position and slice instance number.  If these are both absent then the file's time
   * may be used.
   *
   * @param al Use values from here.
   * @return
   */
  private def getSortingText(al: AttributeList): String = {
    val position = al.get(TagFromName.ImagePositionPatient)
    val positionText =
      if (position == null) ""
      else position.getDoubleValues.map(d => ((d + 10000.0) * 1000).toLong).formatted("%020d").mkString(" ")

    val instanceNumber = al.get(TagFromName.InstanceNumber)
    val instanceText =
      if (instanceNumber == null) ""
      else position.getIntegerValues.map(i => i.formatted("%010d")).mkString(" ")

    positionText + instanceText
  }


  def constructDicomFile(al: AttributeList, sourceFile: File) = {
    new DicomFile(FORMap.getForList(al), sourceFile, TreeUtil.getDateTime(al), getSortingText(al))
  }
}

