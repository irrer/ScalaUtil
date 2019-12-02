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

  object QueryRetrieveLevel extends Enumeration {
    val STUDY = Value
    val SERIES = Value
    val IMAGE = Value
  }

  object QueryRetrieveInformationModel extends Enumeration {
    val StudyRoot = Value
    val PatientRoot = Value
    val PatientStudyOnly = Value

    def sopClassOf(qrim: QueryRetrieveInformationModel.Value) = {
      qrim match {
        case StudyRoot => SOPClass.StudyRootQueryRetrieveInformationModelFind
        case PatientRoot => SOPClass.PatientRootQueryRetrieveInformationModelFind
        case PatientStudyOnly => SOPClass.PatientStudyOnlyQueryRetrieveInformationModelFind
      }
    }
  }

  private val defaultQueryInformationModel = QueryRetrieveInformationModel.StudyRoot //  SOPClass.StudyRootQueryRetrieveInformationModelFind
  private val defaultQueryLevel = QueryRetrieveLevel.SERIES

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
    queryLevel: QueryRetrieveLevel.Value = QueryRetrieveLevel.SERIES,
    limit: Option[Int] = None,
    queryRetrieveInformationModel: QueryRetrieveInformationModel.Value = defaultQueryInformationModel): Seq[AttributeList] = {

    val affectedSOPClass = QueryRetrieveInformationModel.sopClassOf(queryRetrieveInformationModel)
    // use a copy so as not to modify caller's copy
    val attrList = DicomUtil.clone(attributeList)

    // ensure that the query level is given
    val queryLevelAt = AttributeFactory.newAttribute(TagFromName.QueryRetrieveLevel)
    queryLevelAt.addValue(queryLevel.toString)
    attrList.put(queryLevelAt)

    try {
      val association = FindSOPClassSCU.getSuitableAssociation(calledPacs.host, calledPacs.port, calledPacs.aeTitle, callingAETitle, affectedSOPClass)

      val identHandler = new IdentHandler(limit, association)
      val findSOPClassSCU = new FindSOPClassSCU(association, affectedSOPClass, attrList, identHandler)
      val list = identHandler.get
      Trace.trace("releasing C-FIND association")
      association.release
      list
    } catch {
      case t: Throwable =>
        logger.info("Problem: " + fmtEx(t)) // TODO make better
        Seq[AttributeList]()
    }

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
      a.addValue("RTPLAN")
      //a.addValue("CT")
      //a.addValue("REG")
      //a.addValue("RTRECORD")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.SOPInstanceUID)
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.NumberOfBeams)
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.RTPlanTime)
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.RTPlanLabel)
      //      a.addValue("20190628")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.PatientName)
      //      a.addValue("20190628")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.InstanceCreationDate)
      //      a.addValue("20190628")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.SeriesInstanceUID)
      //a.addValue("1.2.246.352.62.2.5521482476547391701.18195090857756125851")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.SeriesDescription)
      //      a.addValue("20190628")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.StudyInstanceUID)
      //      a.addValue("20190628")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.StudyDescription)
      //      a.addValue("20190628")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.RTPlanDate)
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.RTPlanDescription)
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.StudyDate)
      //      a.addValue("20160214")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.AcquisitionDate)
      //      a.addValue("20190517")
      al.put(a)
    }

    if (true) {
      val a = AttributeFactory.newAttribute(TagFromName.ContentDate)
      //      a.addValue("20190517")
      al.put(a)
    }

    println("query:\n--------------------------------\n" + al + "--------------------------------")

    for (qrl <- QueryRetrieveLevel.values; qrim <- QueryRetrieveInformationModel.values) {
      println("query level: " + qrl + "    query retrieve info model: " + qrim)
      val resultList = cfind(
        callingAETitle, // callingAETitle
        calledPacs, // calledPacs
        al, // attributeList
        qrl, // queryLevel
        Some(10), // limit
        qrim)

      println("Number of results: " + resultList.size)

      println(resultList.map(r => r.toString.replace('\0', ' ')).mkString("\n"))
      println("\nNumber of results: " + resultList.size)
      println("-----------------------------------------------------------------------------------------")
      println("-----------------------------------------------------------------------------------------")
      Thread.sleep(500)
    }

    val elapsed = System.currentTimeMillis - start
    println("Done.  Elapsed time in ms: " + elapsed)

  }

}
