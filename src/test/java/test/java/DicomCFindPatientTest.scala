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

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.network.IdentifierHandler
import edu.umro.ScalaUtil.DicomCFindPatient
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PACS

/**
  * Test DicomCFindSeriesForPatient
  */

object DicomCFindPatientTest extends IdentifierHandler with Logging {

  private def al2Human(result: AttributeList): String = {

    def attr2String(attr: Attribute): String = {
      val value = {
        val valueList = attr.getStringValues

        if ((valueList == null) || valueList.isEmpty)
          "<null>"
        else {
          valueList.mkString(" \\ ")
        }
      }

      val tagName = DicomUtil.dictionary.getNameFromTag(attr.getTag)

      val text = s"$tagName : $value"
      text
    }
    val resultText = result.values().toArray.toSeq.filter(_ != null).map(_.asInstanceOf[Attribute]).map(attr2String).mkString("  |  ")

    resultText
  }

  private def showResultList(resultList: Seq[AttributeList]): Unit = {
    if (resultList.nonEmpty) {
      println("\nNumber of results: " + resultList.size)

      println(resultList.take(10).map(r => al2Human(r)).mkString("\n"))

      println("\nNumber of results: " + resultList.size)

      println("-----------------------------------------------------------------------------------------")

      println("PatientID List:\n    " + resultList.map(_.get(TagFromName.PatientID).getSingleStringValueOrEmptyString()).distinct.mkString("\n    "))
    } else
      println("Zero results")
  }

  def main(args: Array[String]): Unit = {

    val PatientID =
      if (args.nonEmpty)
        args.head
      else
        "$AQA_*"

    val startSingle = System.currentTimeMillis

    val callingAETitle = "IRRER"
    val calledPacs = new PACS("VMSDBD", "10.30.65.100", 105)

    val rec = new DicomCFindPatient(callingAETitle, calledPacs)

    // ----------------------------------------------------------------------------------------

    // test single find

    val expectedSize: Int = if (true) {
      val resultList = rec.findByPatient(PatientID)

      val elapsedSingle = System.currentTimeMillis - startSingle
      println("Done with single find.  Elapsed time in ms: " + elapsedSingle + "    Number of results: " + resultList.size)

      showResultList(resultList)

      resultList.size
    } else
      -1

    // ----------------------------------------------------------------------------------------

    // test single find with Modality specified.

    if (true) {
      val resultList = rec.findByPatient(PatientID)

      val elapsedSingle = System.currentTimeMillis - startSingle
      println(s"Done with single find with $PatientID.  Elapsed time in ms: $elapsedSingle    Number of results: ${resultList.size}")

      showResultList(resultList)

      resultList.size
    }

    // ----------------------------------------------------------------------------------------

    // test multiple finds

    val startMulti = System.currentTimeMillis()
    val count = 20
    (0 until count).foreach(_ => {
      val r = rec.findByPatient(PatientID)
      if (r.size != expectedSize)
        throw new RuntimeException(s"Failure.  Expected $expectedSize results but got ${r.size}")
    })
    val elapsedMulti = System.currentTimeMillis() - startMulti
    println(s"Done with multiple find.  Elapsed time in ms: $elapsedMulti    Count: $count    ms/find: ${elapsedMulti.toDouble / count}    find/sec: ${(count * 1000.0) / elapsedMulti}")

    // ----------------------------------------------------------------------------------------

    rec.close()
    println("Done.")
    System.exit(0)
  }

}
