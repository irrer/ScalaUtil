/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.umro.ScalaUtil.DicomSort

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

case class Series(SeriesInstanceUID: String, Modality: String) {
  private val dicomFileList = scala.collection.mutable.Map[String, DicomFile]()

  private var seriesDescription: Option[String] = None

  val uniqueId: Int = TreeUtil.uniqueInt()

  private var uniqueInteger = 0

  def makeUniqueId(): Int =
    uniqueInteger.synchronized {
      uniqueInteger = uniqueInteger + 1
      uniqueInteger
    }

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
      val tagList = Seq(TagByName.SeriesDescription, TagByName.RTImageDescription, TagByName.RTPlanLabel, TagByName.RTPlanName)

      def get(tag: AttributeTag) = {
        val attr = al.get(tag)
        if (attr == null) None
        else {
          val text = attr.getSingleStringValueOrNull
          if ((text != null) && text.trim.nonEmpty) Some(text)
          else None
        }
      }

      seriesDescription = tagList.flatMap(tag => get(tag)).headOption
    }
  }

  /**
    * Add a file by putting the related information into the data structures.
    *
    * @param sourceFile File to add
    * @param al         Attribute list reflecting contents of file.
    */
  def add(sourceFile: File, al: AttributeList): Unit = {
    setDescription(al)
    val SOPInstanceUID = TreeUtil.getAttr(al, TagByName.SOPInstanceUID)
    if (dicomFileList.contains(SOPInstanceUID)) {
      println("\nFile with duplicate SOPInstanceUID ignored: " + sourceFile.getAbsolutePath + "  Previous file: " + dicomFileList(SOPInstanceUID).sourceFile.getAbsolutePath)
    } else dicomFileList.put(SOPInstanceUID, DicomFile.constructDicomFile(al, sourceFile, this))
  }

  /**
    * Get date of series, which is the minimum date of all of the files in the series.
    *
    * @return Earliest date.
    */
  def dateOf(): Date = {
    val date = dicomFileList.values.minBy(df => df.date.getTime).date
    date
  }

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

      val fileName = {
        val base =
          parentDateFormat.format(dateOf()) + "_" +
            Modality + "_" +
            TreeUtil.forToString(forList()) + dicomFileList.values.head.getSpecialName

        //noinspection RegExpSimplifiable
        val trimmed = base.replaceAll("___*", "_").replaceAll("^_", "").replaceAll("_$", "")

        trimmed + "-" + TreeUtil.uniqueInt() + DicomSort.dicomFileSuffix
      }
      val newFile = new File(parentDir, fileName)
      TreeUtil.renameFile(dicomFileList.values.head.sourceFile, newFile)
    } else {
      // There are multiple DICOM files in this series, so create a series directory and put the DICOM files in it.

      val indexFormat = "%0" + dicomFileList.size.toString.length + "d"

      val desc: String = {
        if (seriesDescription.isDefined) "_" + TreeUtil.formatDescription(seriesDescription.get, DicomSort.maxSeriesDescriptionSize)
        else ""
      }

      val forText: String = {
        val forList = dicomFileList.values.flatMap(df => df.forInInst).toIndexedSeq.distinct.sorted
        TreeUtil.forToString(forList)
      }

      val seriesDir = new File(parentDir, parentDateFormat.format(dateOf()) + "_" + Modality + dicomFileList.size + forText + desc + "-" + uniqueId)
      seriesDir.mkdirs()

      def moveFile(df: DicomFile, index: Int): Unit = {
        val fileName = Modality + "_" + (index + 1).formatted(indexFormat) + df.getSpecialName + "-" + uniqueId.formatted(indexFormat) + DicomSort.dicomFileSuffix
        val newFile = new File(seriesDir, fileName)
        TreeUtil.renameFile(df.sourceFile, newFile)
      }

      val sortedList = dicomFileList.values.toIndexedSeq.sortBy(df => df.sortingText + TreeUtil.standardDateFormat.format(df.date))
      sortedList.zipWithIndex.foreach(dfIndex => moveFile(dfIndex._1, dfIndex._2))
    }
  }

}
