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


package test.java;

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName

/**
 * Test DicomUtil.
 *
 */

class TestDicomUtil extends FlatSpec with Matchers {

  val file = new File("""src\test\resources\rtplan.dcm""")
  println("Using DICOM file " + file.getAbsolutePath)
  val source = new AttributeList
  source.read(file)

  "good" should "be good" in {

    val tagSet = Set(
      TagByName.RTBeamLimitingDeviceType,
      TagFromName.PatientID,
      TagByName.AbortFlag,
      TagByName.CumulativeDoseReferenceCoefficient)

    val attrList = DicomUtil.findAll(source, tagSet)

    def sizeOf(tag: AttributeTag) = attrList.filter(at => at.getTag.compareTo(tag) == 0).size

    //println("attrList:\n" + attrList.mkString("\n").replace('\0', ' '))

    println("Total number of tags found: " + attrList.size)

    attrList.size should be(432)
    sizeOf(TagByName.RTBeamLimitingDeviceType) should be(253)
    sizeOf(TagFromName.PatientID) should be(1)
    sizeOf(TagByName.AbortFlag) should be(0)
    sizeOf(TagByName.CumulativeDoseReferenceCoefficient) should be(178)
  }

  "roundtrip" should "get same data" in {

    val fileList = Seq(
      new File("""src\test\resources\rtplan.dcm"""),
      new File("""src\test\resources\vessel_a.dcm"""),
      new File("""src\test\resources\vessel_b.dcm"""))

    val alList = fileList.map(f => { val al = new AttributeList; al.read(f); al })

    val byteArray = DicomUtil.dicomToZippedByteArray(alList)
    val alRound = DicomUtil.zippedByteArrayToDicom(byteArray)

    val sopBefore = alList.map(al => al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString).mkString(" ")
    println("Round tripped DICOM size: " + alRound.size)
    println("Round tripped DICOM:\n" + alRound.head.toString.replace('\u0000', ' ').take(500))
    println("Round tripped DICOM size: " + alRound.size)
    println("SOPInstanceUID list: " + sopBefore)
    val sopAfter = alRound.map(al => al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString).mkString(" ")
    sopBefore should be(sopAfter)

  }

  "mixed data" should "get just DICOM and ignore non-DICOM" in {
    // create zip containing DICOM and some non-DICOM content
    val dir = new File("""src\test\resources""")
    val data = FileUtil.readFileTreeToZipByteArray(Seq(dir))

    var countOfDicomFiles = 0
    def countDicom(file: File): Unit = {
      if (file.isDirectory) {
        file.listFiles.map(f => countDicom(f))
      } else if (file.getName.toLowerCase.endsWith(".dcm"))
        countOfDicomFiles = countOfDicomFiles + 1
    }
    countDicom(dir)

    val alSeq = DicomUtil.zippedByteArrayToDicom(data)
    Trace.trace("countOfDicomFiles: " + countOfDicomFiles)
    Trace.trace("alSeq.size: " + alSeq.size)

    alSeq.size should be(countOfDicomFiles)

  }
}
