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

package test.java

import java.io.File
import edu.umro.util.Utility
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TransferSyntax
import java.util.Date
import com.pixelmed.dicom.OtherByteAttribute
import com.pixelmed.dicom.OtherWordAttribute
import java.io.FileOutputStream
import com.pixelmed.dicom.FileMetaInformation
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.OtherByteAttributeOnDisk
import com.pixelmed.dicom.Attribute

object Janell {

  val transferSyntax = TransferSyntax.ImplicitVRLittleEndian

  private def readFile(file: File) = {
    val al = new AttributeList
    println("reading file: " + file.getAbsolutePath)
    al.read(file)
    al
  }

  val outStructFile = new File("""D:\tmp\janell\new\newStruct.dcm""")

  val rtstruct = {
    readFile(new File("""D:\tmp\janell\exported\UTSW_SSET_REVIEWED_POTENC_202.dcm"""))
  }

  val translate: Map[String, String] = {
    val series = Seq(("1.3.6.1.4.1.22361.17483843788635.454880692.1570630838261.3", "1.3.12.2.1107.5.1.4.100085.30000019100213005033200000336"))
    val frameOfRef = Seq(("1.3.6.1.4.1.22361.17483843788635.454880692.1570630838261.4", "1.3.12.2.1107.5.1.4.100085.30000019100213030088500000008"))
    val structSop = Seq(("1.2.246.352.222.400.4019590799.10516.1570646097.434", UMROGUID.getUID))
    val structSeries = Seq(("1.2.246.352.222.400.4019590799.10516.1570646102.435", UMROGUID.getUID))

    val instance = JanellSopUids.before.zip(JanellSopUids.after)
    (series ++ frameOfRef ++ structSop ++ structSeries ++ instance).toMap
  }

  val tagSet = Set(
    TagFromName.FrameOfReferenceUID,
    TagFromName.MediaStorageSOPInstanceUID,
    TagFromName.ReferencedFrameOfReferenceUID,
    TagFromName.ReferencedSOPInstanceUID,
    TagFromName.SeriesInstanceUID,
    TagFromName.SOPInstanceUID)

  private def writeFile(al: AttributeList, file: File) = {
    FileMetaInformation.addFileMetaInformation(al, transferSyntax, "JimIrrer")
    DicomUtil.writeAttributeListToFile(al, file, "JimIrrer")
    println("Created " + file.getAbsolutePath)
  }

  val atList = DicomUtil.findAll(rtstruct, tagSet)

  def trans(at: Attribute) = {

    translate.get(at.getSingleStringValueOrEmptyString) match {
      case Some(uid) => {
        at.removeValues
        at.addValue(uid)
      }
      case _ => println("Unrecognized value: " + at)
    }
  }

  def fixSingles = {

    val singleList = Seq(
      //      (TagFromName.PatientName, "xxxxxxxxxx"),
      //      (TagFromName.PatientID, "xxxxxxxxxx"),
      //      (TagFromName.PatientBirthDate, "18000101"),
      //      (TagFromName.PatientSex, "O"),
      (TagFromName.StudyID, "1"))

    singleList.map(s => {
      val at = rtstruct.get(s._1)
      at.removeValues
      at.addValue(s._2)
    })
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    atList.map(at => trans(at))

    fixSingles

    writeFile(rtstruct, outStructFile)

    println("Elapsed ms: " + (System.currentTimeMillis - start))
  }

}