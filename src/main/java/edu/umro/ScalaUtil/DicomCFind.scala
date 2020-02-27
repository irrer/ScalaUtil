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

    def put(tag: AttributeTag): Unit = {
      val a = AttributeFactory.newAttribute(tag)
      al.put(a)
    }

    def putValue(value: String, tag: AttributeTag): Unit = {
      val a = AttributeFactory.newAttribute(tag)
      a.addValue(value)
      al.put(a)
    }

    //    TagFromName.AcquisitionDate
    //    TagFromName.ContentDate
    //    TagFromName.InstanceCreationDate
    //    TagFromName.Modality
    //    TagFromName.NumberOfBeams
    //    TagFromName.PatientID
    //    TagFromName.PatientName
    //    TagFromName.RTPlanDate
    //    TagFromName.RTPlanDescription
    //    TagFromName.RTPlanLabel
    //    TagFromName.RTPlanTime
    //    TagFromName.SeriesDescription
    //    TagFromName.SeriesInstanceUID
    //    TagFromName.SOPInstanceUID
    //    TagFromName.StudyDate
    //    TagFromName.StudyDescription
    //    TagFromName.StudyInstanceUID

    // putValue("1.2.246.352.62.2.4789835203298055753.12840502810399438481", TagFromName.SeriesInstanceUID)
    //putValue("MQATX2OBI2019Q3", TagFromName.PatientID)
    put(TagFromName.PatientID)
    put(TagFromName.SOPInstanceUID)
    //putValue("1.2.246.352.61.2.4683805916517294552.1308322119395909555", TagFromName.SeriesInstanceUID)
    //putValue("1.2.246.352.71.2.427549902257.4634976.20190825123541", TagFromName.SeriesInstanceUID)
    //putValue("1.2.246.352.71.2.824327626427.4631129.20190821171552", TagFromName.SeriesInstanceUID)
    //putValue("1.2.246.352.221.47109383203357140424171245409074821033", TagFromName.SeriesInstanceUID)
    putValue("1.2.246.352.62.2.4789835203298055753.12840502810399438481", TagFromName.SeriesInstanceUID)
    put(TagFromName.Modality)
    put(TagFromName.SeriesDate)
    put(TagFromName.SeriesTime)
    //put(TagFromName.ContentDate)
    //put(TagFromName.ContentTime)
    //putValue("1.2.246.352.61.2.5649017917321910891.9616106119503134379", TagFromName.SeriesInstanceUID)

    //put(TagFromName.Modality)

    println("query:\n--------------------------------\n" + al.toString.replace('\0', ' ') + "--------------------------------")

    //for (qrl <- QueryRetrieveLevel.values; qrim <- QueryRetrieveInformationModel.values) {
    for (qrl <- Seq(QueryRetrieveLevel.IMAGE); qrim <- Seq(QueryRetrieveInformationModel.StudyRoot)) {
      println("query level: " + qrl + "    query retrieve info model: " + qrim)
      val resultList = cfind(
        callingAETitle, // callingAETitle
        calledPacs, // calledPacs
        al, // attributeList
        qrl, // queryLevel
        Some(5000), // limit
        qrim)

      println("Number of results: " + resultList.size)
      //      resultList.map(r => {
      //        val m = r.get(TagFromName.Modality).getSingleStringValueOrEmptyString
      //        val s = r.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString
      //        println(m.formatted("%-10s  ") + s)
      //      })

      println(resultList.map(r => r.toString.replace('\0', ' ')).mkString("\n"))
      println("\nNumber of results: " + resultList.size)
      System.exit(99)
      if (resultList.size > 1) {
        println("Got multiple results")
      }
      println("-----------------------------------------------------------------------------------------")
      println("-----------------------------------------------------------------------------------------")
      Thread.sleep(500)
    }

    val elapsed = System.currentTimeMillis - start
    println("Done.  Elapsed time in ms: " + elapsed)

  }

}
