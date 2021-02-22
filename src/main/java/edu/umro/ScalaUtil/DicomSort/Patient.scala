package edu.umro.ScalaUtil.DicomSort

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName

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
    val StudyInstanceUID = TreeUtil.getAttr(al, TagFromName.StudyInstanceUID)
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
        if (study.getDescription.isDefined) "_" + TreeUtil.formatDescription(study.getDescription.get, DicomSort.maxStudyDescriptionSize)
        else ""
      }
    }

    def dirOf(study: Study) = {
      val studyDir = new File(parentDir, dirNameOf(study))
      studyDir.mkdirs()
      studyDir
    }

    studyList.values.toIndexedSeq.foreach(study => study.move(dirOf(study), dateFormat))
  }
}


