package edu.umro.ScalaUtil

import com.pixelmed.network.IdentifierHandler
import com.pixelmed.dicom.AttributeList
import com.pixelmed.network.FindSOPClassSCU
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.network.Association
import com.pixelmed.network.AReleaseException

/**
 * Support for C-FIND.
 *
 * The possible query retrieve information models are:
 *
 *   StudyRootQueryRetrieveInformationModelFind        = "1.2.840.10008.5.1.4.1.2.2.1"
 *   PatientRootQueryRetrieveInformationModelFind      = "1.2.840.10008.5.1.4.1.2.1.1"
 *   PatientStudyOnlyQueryRetrieveInformationModelFind = "1.2.840.10008.5.1.4.1.2.3.1"
 *
 * The possible query levels are:
 *
 *   PATIENT
 *   STUDY
 *   SERIES
 *   IMAGE
 */

object DicomCFind extends IdentifierHandler with Logging {

  private val defaultQueryInformationModel = SOPClass.StudyRootQueryRetrieveInformationModelFind
  private val defaultQueryLevel = "SERIES"

  /**
   * Handle the incoming C-FIND results.
   */
  private class IdentHandler(limit: Option[Int], association: Association) extends IdentifierHandler {
    private val list = scala.collection.mutable.ArrayBuffer[AttributeList]()

    override def doSomethingWithIdentifier(attributeList: AttributeList) = {
      //logger.trace("Got attributes: " + attributeList.toString().replace('\0', ' '));  // log null chars as blanks
      if (limit.isEmpty || (limit.isDefined && (list.size < limit.get))) {
        list += attributeList
      } else {
        val msg = "C-FIND limit of " + limit.get + " items exceeded.  Shutting down connection."
        logger.warn(msg)
        association.abort
      }
    }

    /**
     * Get the items received.  This is valid whether or not the limit was exceeded.
     */
    def get = list.toSeq
  }

  /**
   * Perform a DICOM C-FIND, returning the list attribute lists.
   *
   * @param callingAETitle: AE title
   *
   * @param calledPacs: query against this PACS
   *
   * @param attributeList: search parameters
   *
   * @param queryLevel: Query level, defaults to SERIES
   *
   * @param limit: If given, stop when this many results have been acquired.
   *
   * @param affectedSOPClass: Query information model.  Defaults to Study Root
   */

  def cfind(callingAETitle: String, calledPacs: PACS, attributeList: AttributeList,
    queryLevel: String = defaultQueryLevel,
    limit: Option[Int] = None,
    affectedSOPClass: String = defaultQueryInformationModel): Seq[AttributeList] = {

    // use a copy so as not to modify caller's copy
    val attrList = DicomUtil.clone(attributeList)

    // ensure that the query level is given
    val queryLevel = AttributeFactory.newAttribute(TagFromName.QueryRetrieveLevel)
    queryLevel.addValue(defaultQueryLevel)
    attrList.put(queryLevel)

    val association = FindSOPClassSCU.getSuitableAssociation(calledPacs.host, calledPacs.port, calledPacs.aeTitle, callingAETitle, affectedSOPClass)

    val identHandler = new IdentHandler(limit, association)
    try {
      new FindSOPClassSCU(association, affectedSOPClass, attrList, identHandler)
    } catch {
      case t: Throwable => logger.info("Problem: " + fmtEx(t)) // TODO make better
    }

    identHandler.get
  }

  def main(args: Array[String]): Unit = {

    val start = System.currentTimeMillis

    // val callingAETitle = "IRRER"
    // val calledPacs = new PACS("VMSDBD_EVAL", "VMSDBD_EVAL", 105)
    val callingAETitle = "WLQA_TEST"
    val calledPacs = new PACS("VMSDBD", "VMSDBD", 105)

    val al = new AttributeList

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.PatientID)
      //a.addValue("000000065")
      //a.addValue("$T1boxblk")
      //a.addValue("0098")
      //a.addValue("QASRSWL1")
      a.addValue("MQATX1OBIQA2019Q3")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.Modality)
      //a.addValue("RTIMAGE")
      //a.addValue("PLAN")
      a.addValue("REG")
      //a.addValue("RTRECORD")
      al.put(a)
    }

    val resultList = cfind(callingAETitle, calledPacs, al, "SERIES", Some(50))

    println("Number of results: " + resultList.size)

    println(resultList.map(r => r.toString.replace('\0', ' ')).mkString("\n"))
    println("\nNumber of results: " + resultList.size)

    val elapsed = System.currentTimeMillis - start
    println("Done.  Elapsed time in ms: " + elapsed)

  }

}
