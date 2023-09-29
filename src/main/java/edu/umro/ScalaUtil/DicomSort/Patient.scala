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

case class Patient(PatientID: String) {
  private val studyList = scala.collection.mutable.Map[String, Study]()

  /**
    * Add a file by putting the related information into the data structures.
    *
    * @param file File to add
    * @param al   Attribute list reflecting contents of file.
    */
  def add(file: File, al: AttributeList): Unit = {
    val StudyInstanceUID = TreeUtil.getAttr(al, TagByName.StudyInstanceUID)
    if (!studyList.contains(StudyInstanceUID)) studyList.put(StudyInstanceUID, Study(StudyInstanceUID))
    studyList(StudyInstanceUID).add(file, al)
  }

  /**
    * Put all of the studies into the given parent directory.
    *
    * @param parentDir Place to put study files.
    */
  def move(parentDir: File): Unit = {
    val prefix = "Study_"
    val dateFormat = TreeUtil.dateTimeFormat(studyList.values.toIndexedSeq.map(s => s.dateOf))

    def dirNameOf(study: Study): String = {
      prefix + dateFormat.format(study.dateOf) + {
        if (study.getDescription.isDefined) "_" + TreeUtil.formatDescription(study.getDescription.get, DicomSort.maxStudyDescriptionSize) + "-" + study.uniqueId
        else ""
      }
    }

    def dirOf(study: Study) = {
      val studyDir = new File(parentDir, dirNameOf(study))
      studyDir.mkdirs()
      studyDir
    }

    studyList.values.toIndexedSeq.foreach(study => study.move(dirOf(study)))
  }
}
