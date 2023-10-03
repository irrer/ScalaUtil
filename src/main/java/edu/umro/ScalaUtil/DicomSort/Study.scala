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
import edu.umro.DicomDict.TagByName

import java.io.File
import java.util.Date

case class Study(StudyInstanceUID: String) {
  private val seriesList = scala.collection.mutable.Map[String, Series]()
  def getSeriesList: Map[String, Series] = seriesList.toMap

  val uniqueId: Int = TreeUtil.uniqueInt()

  private var studyDescription: Option[String] = None

  def getDescription: Option[String] = studyDescription

  /**
    * If there is a study description, then save it.
    *
    * @param al Look here for description.
    */
  private def setDescription(al: AttributeList): Unit = {
    if (studyDescription.isEmpty) {
      val at = al.get(TagByName.StudyDescription)
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
    val SeriesInstanceUID = TreeUtil.getAttr(al, TagByName.SeriesInstanceUID)
    if (!seriesList.contains(SeriesInstanceUID)) seriesList.put(SeriesInstanceUID, Series(SeriesInstanceUID, TreeUtil.getAttr(al, TagByName.Modality)))
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
  def move(parentDir: File): Unit = {

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
