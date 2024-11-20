/*
 * Copyright 2024 Regents of the University of Michigan
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

package edu.umro.ScalaUtil.dicomCMove

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TagFromName
import com.pixelmed.network.Association
import com.pixelmed.network.MoveSOPClassSCU
import edu.umro.ScalaUtil.DicomCliUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PACS
import org.apache.commons.cli.CommandLine
import org.apache.commons.cli.Option

import java.io.Closeable
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.tailrec

/**
  * Copy DICOM files from other systems to this one using C-MOVE.  This contains methods that initiate C-MOVEs.
  *
  * Warning: This class creates a local DICOM association that contains a socket, and therefore persists until
  * closed.  This means that instances of this object will remain instantiated (not garbage collected) even
  * though they may no longer be referenced.  When done with this object, the caller should call the close method.
  *
  * Note: Use of the query information models and query retrieve levels may be platform dependent.  It was found that the ones
  * used here worked against the Varian VMSDBD daemon and Conquest, but other PACS devices may have different requirements.
  *
  * For reference:
  *
  * Information models:
  * StudyRootQueryRetrieveInformationModelMove        : 1.2.840.10008.5.1.4.1.2.2.2
  * PatientRootQueryRetrieveInformationModelMove      : 1.2.840.10008.5.1.4.1.2.1.2
  * PatientStudyOnlyQueryRetrieveInformationModelMove : 1.2.840.10008.5.1.4.1.2.3.2
  *
  * 0008,0052 QueryRetrieveLevel:
  * STUDY
  * SERIES
  * IMAGE
  *
  * @param srcPacs            Source PACS that will be sending files.
  * @param dicomCMoveReceiver DICOM receiver.
  */
class DicomCMoveGetter(srcPacs: PACS, dicomCMoveReceiver: DicomCMoveReceiver) extends Logging with Closeable {

  private case class TagValue(tag: AttributeTag, value: String) {
    def toAttribute: Attribute = {
      val attribute = AttributeFactory.newAttribute(tag)
      attribute.addValue(value)
      attribute
    }
  }

  private def makeSpecification(list: Seq[TagValue]): AttributeList = {
    val al = new AttributeList
    list.map(tv => al.put(tv.toAttribute))
    al
  }

  /**
    * Association for starting C-MOVE.  Created and released as necessary.
    */
  private var association: scala.Option[Association] = None

  private def makeAssociation: Association = {
    MoveSOPClassSCU.getSuitableAssociation(srcPacs.host, srcPacs.port, srcPacs.aeTitle, dicomCMoveReceiver.thisPacs.aeTitle, SOPClass.PatientRootQueryRetrieveInformationModelMove)
  }

  /**
    * Make a new subdirectory for receiving DICOM files.
    *
    * @return New subdirectory.
    */
  @tailrec
  private def makeUniqueSubDir(specification: AttributeList): File = {

    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss.SSS")

    /* make directory name with
     *    date + time of C-MOVE
     *    source PACS
     *    source Query level
     *    query parameters
     */
    val name = {
      val time = edu.umro.ScalaUtil.Util.formatDate(dateFormat, new Date)
      val level = specification.get(TagFromName.QueryRetrieveLevel).getSingleStringValueOrEmptyString()
      val src = srcPacs.aeTitle
      val id = {
        val list1 = specification.values().toArray().map(_.asInstanceOf[Attribute])
        val list2 = list1.filterNot(a => a.getTag.equals(TagFromName.QueryRetrieveLevel))
        list2.map(_.getSingleStringValueOrEmptyString()).mkString("_")
      }
      val fullName = {
        // @formatter:off
        val n1 = s"$time $src $level $id"                            // identifying fields.
        val n2 = n1.replace('\u0000', '_')                           // remove nulls
        val n3 = FileUtil.replaceInvalidFileNameCharacters(n2, '_')  // only allow characters that can be used in file names
        val n4 = n3.replaceAll(" ", "_")                             // no spaces
        val n5 = n4.replaceAll("__+", "_")                           // shorten multiple underscores to singles
        // @formatter:on
        n5
      }
      fullName
    }

    val dir = new File(dicomCMoveReceiver.mainDir, name)

    if (dir.exists()) {
      Thread.sleep(100)
      makeUniqueSubDir(specification)
    } else {
      dir.mkdirs()
      dir
    }

  }

  /**
   * General C-MOVE the copies files from the destination to this PACS.
   *
   * @param specification    Indicates which instance, series, or patient.
   * @param affectedSOPClass Affected class.  In practice the remote PACS seems to allow either of:
   *                         StudyRootQueryRetrieveInformationModelMove        : 1.2.840.10008.5.1.4.1.2.2.2;
   *                         PatientRootQueryRetrieveInformationModelMove      : 1.2.840.10008.5.1.4.1.2.1.2;
   *
   *                         but not
   *
   *                         PatientStudyOnlyQueryRetrieveInformationModelMove : 1.2.840.10008.5.1.4.1.2.3.2;
   * @return Result with destination directory.
   */
  private def cMove(
                     specification: AttributeList,
                     //noinspection SameParameterValue
                     affectedSOPClass: String
                   ): CMoveResult = {
    val subDir = makeUniqueSubDir(specification)
    dicomCMoveReceiver.setSubDir(subDir)
    val specAsString = specification.toString.replace('\u0000', ' ')

    // If no QueryRetrieveLevel is specified, then fail badly.
    if (specification.get(TagFromName.QueryRetrieveLevel) == null) {
      throw new RuntimeException("No QueryRetrieveLevel specified")
    }

    logger.info("Starting C-MOVE of files from " + srcPacs.aeTitle + " to " + dicomCMoveReceiver.thisPacs.aeTitle + " with specification of \n" + specAsString)
    try {

      if (association.isEmpty)
        association = Some(makeAssociation)

      val moveSOPClassSCU = new MoveSOPClassSCU(association.get, dicomCMoveReceiver.thisPacs.aeTitle, affectedSOPClass, specification)

      val status = moveSOPClassSCU.getStatus
      logger.info("Completed C-MOVE.  status: " + status + "  hex: " + status.formatted("%x"))
      CMoveResult(None, subDir)
    } catch {
      case e: Exception =>
        val msg = s"CMove error: ${fmtEx(e)}"
        CMoveResult(Some(msg), subDir)
    }
  }

  /**
   * Get the given instance (one DICOM file, e.g. "slice" or "image").
   *
   * @param SOPInstanceUID For the instance to be gotten.
   * @return On success return the directory containing the file.  On failure, return an error message.
   */
  def getInstance(SOPInstanceUID: String): CMoveResult = {

    val specification = makeSpecification(
      Seq(
        TagValue(TagFromName.QueryRetrieveLevel, "IMAGE"),
        TagValue(TagFromName.SOPInstanceUID, SOPInstanceUID)
      )
    )

    val result = cMove(specification: AttributeList, SOPClass.PatientRootQueryRetrieveInformationModelMove)

    result
  }

  /**
   * Get all files for the given series.
   *
   * @param SeriesInstanceUID For the series to be gotten.
   * @return On success return the directory containing the files.  On failure, return an error message.
   */
  def getSeries(SeriesInstanceUID: String): CMoveResult = {

    val specification = makeSpecification(
      Seq(
        TagValue(TagFromName.QueryRetrieveLevel, "SERIES"),
        TagValue(TagFromName.SeriesInstanceUID, SeriesInstanceUID)
      )
    )

    val result = cMove(specification: AttributeList, SOPClass.PatientRootQueryRetrieveInformationModelMove)

    result
  }

  /**
   * Get all files for the given patient.
   *
   * @param PatientID For the series to be gotten.
   * @return On success return the directory containing the files.  On failure, return an error message.
   */
  def getPatient(PatientID: String): CMoveResult = {

    val specification = makeSpecification(
      Seq(
        TagValue(TagFromName.QueryRetrieveLevel, "PATIENT"), // Either SERIES PATIENT works
        TagValue(TagFromName.PatientID, PatientID)
      )
    )

    val result = cMove(specification: AttributeList, SOPClass.PatientRootQueryRetrieveInformationModelMove)

    result
  }

  /**
   * If the association is open, then close it.
   */
  override def close(): Unit = {
    if (association.nonEmpty) {
      try {
        association.get.release()
      }
      catch {
        case t: Throwable =>
          logger.warn(s"Unexpected exception closing C-MOVE association: ${fmtEx(t)}")
      }
      finally {
        association = None
      }
    }
  }
}

object DicomCMoveGetter extends Logging {

  /*
  private case class AssociationItem(key: String, association: Association) {}

  private val associationPool = scala.collection.mutable.HashMap[String, AssociationItem]()

  private def takeAssociation(srcPacs: PACS, thisPacsAETitle: String): AssociationItem = associationPool.synchronized {

    def makeKey() = Seq(srcPacs.host, srcPacs.aeTitle, srcPacs.port.toString, thisPacsAETitle).mkString("  ||||  ")

    val key = makeKey()
    if (associationPool.contains(key)) {
      associationPool.remove(key).get
    }
    else {
      val a = MoveSOPClassSCU.getSuitableAssociation(srcPacs.host, srcPacs.port, srcPacs.aeTitle, thisPacsAETitle, SOPClass.PatientRootQueryRetrieveInformationModelMove)
      AssociationItem(key, a)
    }
  }
  */

  // ---------------------------------------------------------------------------------------------------

  // Command line interface

  private def usage(): Unit = {
    println(
      """Parameters (exactly one of the following):
        |  IMAGE SOPInstanceUID
        |  SERIES SeriesInstanceUID
        |  PATIENT PatientID
        |""".stripMargin)
    System.exit(1)
  }

  def main(args: Array[String]): Unit = {

    val mainDir = new File("""D:\tmp\getter""")
    val thisPacs = new PACS("WLQA_TEST", "141.214.125.209", 5682)

    import DicomCliUtil._
    import org.apache.commons.cli.Options

    val options = new Options()

    addServerOptions(options)

    val optionImage = new Option("I", "IMAGE", true, "get single image with image UID.")
    val optionSeries = new Option("S", "SERIES", true, "get all images in series with series UID.")
    val optionPatient = new Option("P", "PATIENT", true, "get all images for patient with patient ID.")

    options.addOption(optionImage)
    options.addOption(optionSeries)
    options.addOption(optionPatient)

    val commandLine: CommandLine = {
      val cl = parseOptions(options, args)

      if (cl.isDefined) cl.get
      else {
        usage()
        System.exit(1)
        throw new RuntimeException("Unexpected error")
      }
    }

    val srcPacs = getServerPACS(commandLine)

    val start = System.currentTimeMillis()

    val remainingArgs = commandLine.getArgs

    val rec = DicomCMoveReceiver(mainDir, thisPacs)

    val getter = new DicomCMoveGetter(srcPacs, rec)

    val result: CMoveResult = 0 match {
      case _ if commandLine.hasOption(optionImage.getOpt) => getter.getInstance(commandLine.getOptionValue(optionImage.getOpt))
      case _ if commandLine.hasOption(optionSeries.getOpt) => getter.getSeries(commandLine.getOptionValue(optionSeries.getOpt))
      case _ if commandLine.hasOption(optionPatient.getOpt) => getter.getPatient(commandLine.getOptionValue(optionPatient.getOpt))
      case _ =>
        usage()
        throw new RuntimeException("Unexpected state.")
    }

    val elapsed_ms = System.currentTimeMillis() - start

    println(s"C-MOVE processed in ${edu.umro.ScalaUtil.Util.intervalTimeUserFriendly(elapsed_ms)}")

    if (result.errorMessage.isEmpty) {
      val size = FileUtil.listFiles(result.dir).size
      println(s"\n\n$size files received into ${result.dir.getAbsolutePath}\n\n")
      System.exit(0)
    } else {
      println(s"Error: ${result.errorMessage.get}")
      showHelp(options)
      System.exit(1)
    }
  }

}
