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

package edu.umro.ScalaUtil

import com.pixelmed.dicom._
import com.pixelmed.network.ReceivedObjectHandler

import java.io.File

//noinspection ScalaUnusedSymbol
object DicomGetter extends Logging {

  var count = 0

  class DefaultReceivedObjectHandler extends ReceivedObjectHandler {
    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String): Unit = {
      logger.info("Received DICOM from " + callingAETitle + " file " + " DICOM file " + fileName)
      count = count + 1
    }
  }

  def addAttr(tag: AttributeTag, value: String, identifier: AttributeList): AttributeList = {
    val a = AttributeFactory.newAttribute(tag)
    a.addValue(value)
    identifier.put(a)
    identifier
  }

  def buildIdentifier: AttributeList = {
    addAttr(TagFromName.QueryRetrieveLevel, "SERIES", new AttributeList)
  }

  def main(args: Array[String]): Unit = {

    //val mainDirName = """D:\tmp\aqa\tmp\May9_TX4"""
    val mainDirName = """D:\tmp\getter"""

    val mainDir = new File(mainDirName)
    println("Putting files in: " + mainDir.getAbsolutePath)
    mainDir.mkdirs

    val cpXstorPacs = new PACS("CP_XSTOR_IRRER", "141.214.125.209", 15656)
    val irrerPacs = new PACS("IRRER", "141.214.125.209", 15678)
    val umroarchivePacs = new PACS("UMRO_ARCHIVE", "10.30.3.69", 104)
    val evalPacs = new PACS("EVAL1109", "141.214.124.189", 104)
    val wlqaTestPacs = new PACS("WLQA_TEST", "141.214.125.209", 5682)
    val clinPacs = new PACS("VMSDBD", "10.30.65.100", 105)
    val clinPacsX = new PACS("VMSDBD", "10.30.65.100", 1433)

    val thisPacs = wlqaTestPacs
    val thatPacs = clinPacs

    println("localPacs: " + thisPacs)
    println("remotePacs: " + thatPacs)

    val dicomReceiver = new DicomReceiver(mainDir, thisPacs)

    val working = Seq("1.2.246.352.62.2.5035239981428730453.17224928362942256548")

    /**
      * Get DICOM by SeriesInstanceUID.
      */
    def fetchSeries(seriesUid: String): Unit = {
      dicomReceiver.setSubDir(seriesUid)
      val id = buildIdentifier
      val id2 = addAttr(TagFromName.SeriesInstanceUID, seriesUid, id)
      val subDir = new File(mainDir, seriesUid)
      if (subDir.exists) try { edu.umro.util.Utility.deleteFileTree(subDir) }
      catch { case _: Throwable => }

      dicomReceiver.setSubDir(subDir)
      println("Putting files into " + dicomReceiver.getSubDir.getAbsolutePath)
      val start = System.currentTimeMillis
      val success = dicomReceiver.cmove(id, thatPacs, thisPacs) // , SOPClass.PatientRootQueryRetrieveInformationModelMove)
      val elapsed = System.currentTimeMillis - start
      val status = if (success.isDefined) "Failed: " + success.get else "Success"
      println("elapsed ms: " + elapsed + "    Status: " + status)
    }

    /**
      * Get DICOM by SeriesInstanceUID.
      */
    def fetchPatient(patientId: String): Unit = {
      dicomReceiver.setSubDir(patientId)
      val id = buildIdentifier
      addAttr(TagFromName.PatientID, patientId, id)
      // addAttr(TagFromName.Modality, "RTPLAN", id)
      val subDir = new File(mainDir, patientId)
      if (subDir.exists) try {
        edu.umro.util.Utility.deleteFileTree(subDir)
      } catch {
        case _: Throwable =>
      }

      dicomReceiver.setSubDir(subDir)
      println("Putting files into " + dicomReceiver.getSubDir.getAbsolutePath)
      val start = System.currentTimeMillis
      val success = dicomReceiver.cmove(id, thatPacs, thisPacs) // , SOPClass.PatientRootQueryRetrieveInformationModelMove)
      val elapsed = System.currentTimeMillis - start
      val status = if (success.isDefined) "Failed: " + success.get else "Success"
      println("elapsed ms: " + elapsed + "    Status: " + status)
    }

    /**
      * Get DICOM by SOPInstanceUID.
      */
    def fetchInstance(sopUid: String): Unit = {
      dicomReceiver.setSubDir(sopUid)
      val id = buildIdentifier
      val id2 = addAttr(TagFromName.SOPInstanceUID, sopUid, id)
      val subDir = new File(mainDir, sopUid)
      if (subDir.exists) edu.umro.util.Utility.deleteFileTree(subDir)

      dicomReceiver.setSubDir(subDir)
      println("Putting files into " + dicomReceiver.getSubDir.getAbsolutePath)
      val start = System.currentTimeMillis
      val success = dicomReceiver.cmove(id, thatPacs, thisPacs, SOPClass.PatientRootQueryRetrieveInformationModelMove)
      val elapsed = System.currentTimeMillis - start
      val status = if (success.isDefined) "Failed: " + success.get else "Success"
      println("elapsed ms: " + elapsed + "    Status: " + status)
    }

    val start = System.currentTimeMillis

    // ===================================================================================================

    // TODO put query here
    // fetchPatient("008872739")
    // fetchSeries("1.2.246.352.62.2.5668201371073720637.10222042694179389615") // $AQA_TB3 gap skew that seems to be problematic
    fetchInstance("1.2.246.352.71.5.427549902257.981519.20230214102306")
    // fetchSeries("1.2.246.352.62.2.5263594622170571224.8003024706199981243") // random daily QA RTIMAGE set

    // ===================================================================================================

    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
    System.exit(0)
  }

}
