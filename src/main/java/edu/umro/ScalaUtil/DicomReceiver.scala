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

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.StoredFilePathStrategy
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.TransferSyntax
import com.pixelmed.network.MoveSOPClassSCU
import com.pixelmed.network.PresentationContext
import com.pixelmed.network.ReceivedObjectHandler
import com.pixelmed.network.StorageSOPClassSCPDispatcher
import edu.umro.util.Utility

import java.io.File

/**
  * Support for copying files from Varian to the local file system.
  */
class DicomReceiver(mainDir: File, myPacs: PACS, receivedObjectHandler: ReceivedObjectHandler) extends Logging {

  def this(mainDir: File, myPacs: PACS) = this(mainDir, myPacs, new DicomReceiver.DefaultReceivedObjectHandler)

  lazy val mainDirName: String = mainDir.getAbsolutePath

  /** Name of sub-directory to put incoming DICOM files in. */
  private var subDirFile: Option[File] = None

  def getSubDir: File = subDirFile.get

  def setSubDir(subDir: File): File = {
    val sub = subDir.getAbsolutePath
    val main = mainDir.getAbsolutePath
    if (sub.startsWith(main) && (sub.length > main.length))
      this.subDirFile = Some(subDir)
    else
      throw new RuntimeException("Must specify a proper sub-directory of the main directory " + main + "     specified: " + sub)
    getSubDir
  }

  def setSubDir(subDirName: String): File = {
    setSubDir(new File(mainDir + File.separator + subDirName))
  }

  private class StoredFilePathStrategyJobFolders extends StoredFilePathStrategy {
    override def makeStoredFilePath(sopInstanceUID: String): String = {
      val file = new File(getSubDir, sopInstanceUID + ".dcm")
      val path = file.getAbsolutePath.substring(mainDirName.length + 1)
      path
    }
  }

  private def dispatcher = {
    println("Starting dispatcher with PACS " + myPacs)
    new StorageSOPClassSCPDispatcher(
      myPacs.port, // port that we are listening on
      myPacs.aeTitle, // our AETitle
      mainDir, // directory for temporary and fetched files
      new StoredFilePathStrategyJobFolders, // strategy for naming incoming DICOM files
      receivedObjectHandler
    )
  }

  private def getPresentationContext: java.util.LinkedList[PresentationContext] = {
    val presentationContextList = new java.util.LinkedList[PresentationContext]

    val tslist = new java.util.LinkedList[String]
    tslist.add(TransferSyntax.ImplicitVRLittleEndian)
    tslist.add(TransferSyntax.ExplicitVRLittleEndian)
    presentationContextList.add(new PresentationContext(3.toByte, SOPClass.PatientRootQueryRetrieveInformationModelMove, tslist))
    presentationContextList
  }

  /**
    * Perform DICOM C-MOVE.
    *
    * @return true on success
    */
  def cmove(specification: AttributeList, srcPacs: PACS, dstPacs: PACS, affectedSOPClass: String = SOPClass.PatientRootQueryRetrieveInformationModelMove): Option[String] = {
    if (subDirFile.isDefined) {
      subDirFile.get.mkdirs
      val specAsString = specification.toString.replace('\u0000', ' ')

      // If no QueryRetrieveLevel is specified, then fail badly.
      if (specification.get(TagFromName.QueryRetrieveLevel) == null) {
        throw new RuntimeException("No QueryRetrieveLevel specified")
      }

      //val affectedSOPClass = SOPClass.StudyRootQueryRetrieveInformationModelMove
      //val affectedSOPClass = SOPClass.PatientRootQueryRetrieveInformationModelMove

      logger.info("Starting C-MOVE of files from " + srcPacs.aeTitle + " to " + dstPacs.aeTitle + " with specification of \n" + specAsString)
      try {
        val moveSOPClassSCU = new MoveSOPClassSCU(
          srcPacs.host, // source host
          srcPacs.port, // source port
          srcPacs.aeTitle, // source aeTitle
          myPacs.aeTitle, // callingAETitle aeTitle
          dstPacs.aeTitle, // moveDestination
          affectedSOPClass, //  affectedSOPClass
          specification
        ) // identifier
        val status = moveSOPClassSCU.getStatus
        logger.info("Completed C-MOVE.  status: " + status + "  hex: " + status.formatted("%x"))
        None
      } catch {
        case e: Exception =>
          Some("CMove error: " + fmtEx(e))
      }
    } else Some("The subdirectory value must be set before performing a C-MOVE.  Use setSubDir")
  }

  //  @deprecated("Use <code>cmove(specification: AttributeList, srcPacs: PACS, dstPacs: PACS, affectedSOPClass: String)</code> instead.")
  //  def cmove(specification: AttributeList, srcPacs: PACS, dstPacs: PACS): Option[String] = {
  //    cmove(specification: AttributeList, srcPacs: PACS, dstPacs, SOPClass.PatientRootQueryRetrieveInformationModelMove)
  //  }

  /** Start a DICOM receiver. */
  private def startReceiver(): Unit = {
    println("==================================== DicomReceiver.startReceiver") // TODO rm
    val dispatcherThread = new Thread(dispatcher)
    dispatcherThread.start()
    logger.info("Started DICOM receiver: " + myPacs)
  }

  startReceiver()
}

object DicomReceiver extends Logging {

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

    val mainDir = new File("""D:\tmp\archive_migration_from_xstor_to_velocity\valid""")
    println("Putting files in: " + mainDir.getAbsolutePath)
    mainDir.mkdirs

    val cpXstorPacs = new PACS("CP_XSTOR_IRRER", "141.214.125.209", 15656)
    val irrerPacs = new PACS("IRRER", "141.214.125.209", 15678)
    val umroarchivePacs = new PACS("UMRO_ARCHIVE", "10.30.3.69", 104)
    val evalPacs = new PACS("EVAL1109", "141.214.124.189", 104)
    val wlqaTestPacs = new PACS("WLQA_TEST", "141.214.125.209", 5682)
    val clinPacs = new PACS("VMSDBD", "uhroariaspr1", 105)

    val thisPacs = wlqaTestPacs
    val thatPacs = clinPacs

    val dicomReceiver = new DicomReceiver(mainDir, thisPacs)

    //val seriesUID = "1.3.6.1.4.1.22361.48658618118952.539916499.1500572921197.3"
    //val seriesUID = "1.2.840.113704.1.111.7924.1428068730.3"
    //val seriesUID = "1.3.12.2.1107.5.2.19.45228.30000017020216261927100007259"
    //val seriesUID = "1.3.12.2.1107.5.2.19.45228.2016103111160677663408121.2.0.0"
    //val seriesUID = "1.3.12.2.1107.5.2.19.45228.201705181425239536778305.1.0.0"
    //val seriesUID = "1.3.12.2.1107.5.2.19.45228.201606231230353215404761.0.0.0"
    //val seriesUID = "1.3.12.2.1107.5.2.19.45228.2018111612583360918723037.0.0.0"
    //val seriesUID = "1.3.12.2.1107.5.2.19.45228.2018012410263353336317985.0.0.0"
    //val seriesUID = "1.2.246.352.71.2.427549902257.4634976.20190825123541"
    //val seriesUID = "1.2.246.352.71.2.824327626427.4631129.20190821171552"
    //val seriesUID = "1.2.246.352.221.47109383203357140424171245409074821033"
    // val seriesUID = "1.2.246.352.61.2.5649017917321910891.9616106119503134379" // works
    val seriesUID = "1.2.246.352.62.2.5454816943001333014.10902676767571471256"

    //val seriesUID = "1.3.6.1.4.1.22361.17483834219463.1233230305.1566414913462.466"

    // val SOPInstanceUID = "1.2.246.352.63.1.5661800265424807387.1120403681608530336"
    // val SOPInstanceUID = "1.2.246.352.62.2.5129711748376230466.9561518987460938120"

    //val studyInstanceUID = "1.2.840.113704.1.111.5364.1467809429.7"

    //val id = addAttr(TagFromName.SOPInstanceUID, SOPInstanceUID, buildIdentifier)
    val id = addAttr(TagFromName.SeriesInstanceUID, seriesUID, buildIdentifier)
    //val id = addAttr(TagFromName.StudyInstanceUID, studyInstanceUID, buildIdentifier)

    println("localPacs: " + thisPacs)
    println("remotePacs: " + thatPacs)

    //dicomReceiver.setSubDir(SOPInstanceUID)
    dicomReceiver.setSubDir(seriesUID)
    //dicomReceiver.setSubDir(studyInstanceUID)
    println("Putting files into " + dicomReceiver.getSubDir.getAbsolutePath)
    Utility.deleteFileTree(dicomReceiver.getSubDir)
    Seq(
      // SOPClass.StudyRootQueryRetrieveInformationModelMove,
      SOPClass.PatientRootQueryRetrieveInformationModelMove
      // SOPClass.PatientStudyOnlyQueryRetrieveInformationModelMove
    ).map(s => {
      val success = dicomReceiver.cmove(id, thatPacs, thisPacs, s)
      println(s + "    Number of files received: " + count)

      success match {
        case Some(msg) => println("Failed: " + msg)
        case _         => println("success.")
      }
    })

    System.exit(0)
  }

}
