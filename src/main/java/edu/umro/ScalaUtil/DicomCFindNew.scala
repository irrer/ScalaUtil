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
import com.pixelmed.dicom.TagFromName
import com.pixelmed.network.Association
import com.pixelmed.network.FindSOPClassSCU
import com.pixelmed.network.IdentifierHandler
import edu.umro.DicomDict.TagByName

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

object DicomCFindNew extends IdentifierHandler with Logging {

  object QueryRetrieveLevel extends Enumeration {
    val STUDY: QueryRetrieveLevel.Value = Value
    val SERIES: QueryRetrieveLevel.Value = Value
    val IMAGE: QueryRetrieveLevel.Value = Value
  }

  object QueryRetrieveInformationModel extends Enumeration {
    val StudyRoot: QueryRetrieveInformationModel.Value = Value
    //noinspection ScalaWeakerAccess
    val PatientRoot: QueryRetrieveInformationModel.Value = Value
    //noinspection ScalaWeakerAccess
    val PatientStudyOnly: QueryRetrieveInformationModel.Value = Value

    def sopClassOf(qr: QueryRetrieveInformationModel.Value): String = {
      qr match {
        case StudyRoot        => SOPClass.StudyRootQueryRetrieveInformationModelFind
        case PatientRoot      => SOPClass.PatientRootQueryRetrieveInformationModelFind
        case PatientStudyOnly => SOPClass.PatientStudyOnlyQueryRetrieveInformationModelFind
      }
    }
  }

  private val defaultQueryInformationModel = QueryRetrieveInformationModel.StudyRoot //  SOPClass.StudyRootQueryRetrieveInformationModelFind
  // private val defaultQueryLevel = QueryRetrieveLevel.SERIES

  /**
    * Format a query to string.
    * @param query C-FIND query parameters.
    * @return Human-readable string version of query.
    */
  //noinspection ScalaWeakerAccess
  def queryToString(query: AttributeList): String = {
    val text = query.toString().replace('\u0000', ' ').replaceAll("\n", "  ||  ")
    text
  }

  /**
    * Handle the incoming C-FIND results.
    */
  private class IdentHandler(limit: Option[Int], association: Association) extends IdentifierHandler {
    private val list = scala.collection.mutable.ArrayBuffer[AttributeList]()

    override def doSomethingWithIdentifier(attributeList: AttributeList): Unit = {
      //logger.trace("Got attributes: " + attributeList.toString().replace('\u0000', ' '));  // log null chars as blanks
      if (limit.isEmpty || (limit.isDefined && (list.size < limit.get))) {
        list += attributeList
      } else {
        val msg = "C-FIND limit of " + limit.get + " items exceeded.  Shutting down connection."
        logger.warn(msg)
        association.abort()
      }
    }

    /**
      * Get the items received.  This is valid whether the limit was exceeded.
      */
    def get: Seq[AttributeList] = list
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
    * @param queryRetrieveInformationModel Query information model.  Defaults to Study Root
    */
  //noinspection SpellCheckingInspection
  def cfind(
      callingAETitle: String,
      calledPacs: PACS,
      attributeList: AttributeList,
      queryLevel: QueryRetrieveLevel.Value = QueryRetrieveLevel.SERIES,
      limit: Option[Int] = None,
      queryRetrieveInformationModel: QueryRetrieveInformationModel.Value = defaultQueryInformationModel
  ): Seq[AttributeList] = {

    val affectedSOPClass = QueryRetrieveInformationModel.sopClassOf(queryRetrieveInformationModel)
    // use a copy so as not to modify caller's copy
    val query = DicomUtil.clone(attributeList)

    // ensure that the query level is given
    val queryLevelAt = AttributeFactory.newAttribute(TagFromName.QueryRetrieveLevel)
    queryLevelAt.addValue(queryLevel.toString)
    query.put(queryLevelAt)

    try {
      val association = FindSOPClassSCU.getSuitableAssociation(calledPacs.host, calledPacs.port, calledPacs.aeTitle, callingAETitle, affectedSOPClass)

      val identHandler = new IdentHandler(limit, association)
      new FindSOPClassSCU(association, affectedSOPClass, query, identHandler)
      val list = identHandler.get
      association.abort()
      list
    } catch {
      case t: Throwable =>
        logger.warn(s"Unexpected exception for query ${queryToString(query)}: " + fmtEx(t))
        Seq[AttributeList]()
    }
  }

  /**
    * Make an association for getting a list of the slices of a given series.
    * The caller is responsible for disposing of this using its abort method.
    *
    * @param callingAETitle Out AE Title
    * @param calledPacs Remote DICOM device.
    * @param queryRetrieveInformationModel query retrieve information model
    * @return A new association.
    */
  //noinspection ScalaWeakerAccess
  def makeCFindAssociation(
      callingAETitle: String,
      calledPacs: PACS,
      queryRetrieveInformationModel: QueryRetrieveInformationModel.Value = defaultQueryInformationModel
  ): Association = {
    val affectedSOPClass = QueryRetrieveInformationModel.sopClassOf(queryRetrieveInformationModel)
    val association = FindSOPClassSCU.getSuitableAssociation(calledPacs.host, calledPacs.port, calledPacs.aeTitle, callingAETitle, affectedSOPClass)
    association
  }

  /**
    * Make an association for getting a list of images for a series.
    *
    * @param callingAETitle Out AE Title
    * @param calledPacs Remote DICOM device.
    * @return An association for getting a list of images for a series.
    */
  //noinspection ScalaWeakerAccess
  def makeCFindAssociationForImagesInSeries(
      callingAETitle: String,
      calledPacs: PACS
  ): Association = {
    makeCFindAssociation(callingAETitle, calledPacs, DicomCFindNew.QueryRetrieveInformationModel.StudyRoot)
  }

  /**
    * Make an association for getting a list of images for a series.
    *
    * @param callingAETitle Out AE Title
    * @param calledPacs Remote DICOM device.
    * @return An association for getting a list of images for a series.
    */
  //noinspection ScalaWeakerAccess
  def makeCFindAssociationForSeriesInPatient(
      callingAETitle: String,
      calledPacs: PACS
  ): Association = {
    makeCFindAssociation(callingAETitle, calledPacs, DicomCFindNew.QueryRetrieveInformationModel.StudyRoot)
  }

  /**
    * Create a query and populate it with the given tags.  No values are assigned to the tags.
    *
    * @param tagList List of tags.
    * @return query for user with C-FIND.
    */
  //noinspection ScalaWeakerAccess
  def populateQuery(query: Option[AttributeList] = Some(new AttributeList), tagList: Seq[AttributeTag]): AttributeList = {
    val qry = if (query.isDefined) query.get else new AttributeList
    def put(tag: AttributeTag): Unit = {
      val attr = AttributeFactory.newAttribute(tag)
      qry.put(attr)
    }
    tagList.foreach(put)
    qry
  }

  /**
    * Make a query for getting a list of images for a series.
    *
    * The query is populated with a list of tags whose values may or may not be returned.
    *
    * It is usually not necessary to call this function externally, but is provided for cases where
    * the remote PACS can provide other information regarding each slice.
    *
    * @param SeriesInstanceUID For this series UID.
    * @return Query for getting a list of images for a series.
    */
  //noinspection ScalaWeakerAccess
  def makeMinimalCFindQueryForImagesInSeries(SeriesInstanceUID: Option[String] = None): AttributeList = {

    val query = new AttributeList

    val instanceUidAttr = AttributeFactory.newAttribute(TagByName.SOPInstanceUID)
    query.put(instanceUidAttr)

    val seriesUidAttr = AttributeFactory.newAttribute(TagByName.SeriesInstanceUID)
    if (SeriesInstanceUID.isDefined)
      seriesUidAttr.addValue(SeriesInstanceUID.get)
    query.put(seriesUidAttr)

    val queryLevelAttr = AttributeFactory.newAttribute(TagByName.QueryRetrieveLevel)
    queryLevelAttr.addValue(QueryRetrieveLevel.IMAGE.toString)
    query.put(queryLevelAttr)

    query
  }

  /**
    * Make a query for getting a list of images for a series.
    *
    * The query is populated with a list of tags whose values may or may not be returned.
    *
    * It is usually not necessary to call this function externally, but is provided for cases where
    * the remote PACS can provide other information regarding each slice.
    *
    * @param SeriesInstanceUID For this series UID.
    * @return Query for getting a list of images for a series.
    */
  //noinspection ScalaWeakerAccess
  def makeCFindQueryForImagesInSeries(SeriesInstanceUID: Option[String] = None): AttributeList = {

    val query = populateQuery(
      Some(makeMinimalCFindQueryForImagesInSeries(SeriesInstanceUID)),
      Seq(
        TagFromName.InstanceCreationDate,
        TagFromName.InstanceCreationTime,
        TagFromName.SOPInstanceUID,
        TagFromName.AcquisitionDate,
        TagFromName.ContentDate,
        TagFromName.AcquisitionTime,
        TagFromName.ContentTime,
        TagFromName.Modality,
        TagFromName.PatientName,
        TagFromName.PatientID,
        TagFromName.PatientPosition,
        TagFromName.SeriesInstanceUID,
        TagFromName.InstanceNumber,
        TagFromName.PatientOrientation,
        TagFromName.FrameOfReferenceUID,
        TagFromName.SeriesDate,
        TagFromName.SeriesTime
      )
    )

    query
  }

  /**
    * Make a query for getting a list of images for a series.
    *
    * The query is populated with a list of tags whose values may or may not be returned.
    *
    * It is usually not necessary to call this function externally, but is provided for cases where
    * the remote PACS can provide other information regarding each slice.
    *
    * @param PatientID For this series UID.
    * @return Query for getting a list of images for a series.
    */
  //noinspection ScalaWeakerAccess
  def makeMinimalCFindQueryForSeriesInPatient(PatientID: Option[String] = None): AttributeList = {

    val query = new AttributeList

    val instanceUidAttr = AttributeFactory.newAttribute(TagByName.SOPInstanceUID)
    query.put(instanceUidAttr)

    val seriesUidAttr = AttributeFactory.newAttribute(TagByName.SeriesInstanceUID)
    if (PatientID.isDefined)
      seriesUidAttr.addValue(PatientID.get)
    query.put(seriesUidAttr)

    val queryLevelAttr = AttributeFactory.newAttribute(TagByName.QueryRetrieveLevel)
    queryLevelAttr.addValue(QueryRetrieveLevel.SERIES.toString)
    query.put(queryLevelAttr)

    query
  }

  /**
    * Make a query for getting a list of series for a patient.
    *
    * The query is populated with a list of tags whose values may or may not be returned.
    *
    * It is usually not necessary to call this function externally, but is provided for cases where
    * the remote PACS can provide other information regarding each slice.
    *
    * @param PatientID For this series UID.
    * @return Query for getting a list of images for a series.
    */
  //noinspection ScalaWeakerAccess
  def makeCFindQueryForSeriesInPatient(PatientID: Option[String] = None): AttributeList = {

    val query = populateQuery(
      Some(makeMinimalCFindQueryForSeriesInPatient(PatientID)),
      Seq(
        TagFromName.SOPClassUID,
        TagFromName.StudyDate,
        TagFromName.SeriesDate,
        TagFromName.StudyTime,
        TagFromName.SeriesTime,
        TagFromName.AccessionNumber,
        TagFromName.Modality,
        TagFromName.Manufacturer,
        TagFromName.InstitutionName,
        TagFromName.StationName,
        TagFromName.StudyDescription,
        TagFromName.SeriesDescription,
        TagFromName.OperatorsName,
        TagFromName.ManufacturerModelName,
        TagFromName.PatientName,
        TagFromName.PatientBirthDate,
        TagFromName.DeviceSerialNumber,
        TagFromName.SoftwareVersions,
        TagFromName.PatientPosition,
        TagFromName.StudyInstanceUID,
        TagFromName.SeriesInstanceUID,
        TagFromName.StudyID,
        TagFromName.SeriesNumber,
        TagFromName.PatientOrientation,
        TagFromName.FrameOfReferenceUID
      )
    )

    query
  }

  /**
    * Get the list of images for a given series.
    *
    * If the limit parameter is specified and the limit is exceeded, then the association is closed.
    *
    * @param association Association with remote PACS. See  <code>makeCFindAssociation</code>.
    * @param query List of metadata for each slice.
    * @param queryLevel Query level.
    * @param limit Stop if this many slices received.
    * @return
    */
  //noinspection ScalaWeakerAccess
  def cFind2(
      association: Association,
      query: AttributeList,
      queryLevel: QueryRetrieveLevel.Value = QueryRetrieveLevel.SERIES,
      limit: Option[Int] = None
  ): Seq[AttributeList] = {
    val affectedSOPClass = QueryRetrieveInformationModel.sopClassOf(defaultQueryInformationModel)

    // use a copy so as not to modify caller's copy
    val newQuery = DicomUtil.clone(query)

    // ensure that the query level is given
    val queryLevelAt = AttributeFactory.newAttribute(TagFromName.QueryRetrieveLevel)
    queryLevelAt.addValue(queryLevel.toString)
    newQuery.put(queryLevelAt)

    try {
      val identHandler = new IdentHandler(limit, association)
      new FindSOPClassSCU(association, affectedSOPClass, newQuery, identHandler)
      // findSOPClassSCU.performFind(association, affectedSOPClass, query)
      val list = identHandler.get
      list
    } catch {
      case t: Throwable =>
        logger.info("Problem: " + fmtEx(t))
        Seq[AttributeList]()
    }
  }

  /**
    * Get the list of images for a given series.  An association is created and closed.
    *
    * @param callingAETitle Out AE Title
    * @param calledPacs Remote DICOM device.
    * @param SeriesInstanceUID For this series.
    * @param query List of attributes to retrieve.
    * @param limit Stop if this manu items are returned.
    * @return
    */
  //noinspection ScalaWeakerAccess
  def cFindImagesInSeries(
      callingAETitle: String,
      calledPacs: PACS,
      SeriesInstanceUID: String,
      query: AttributeList = makeCFindQueryForImagesInSeries(),
      limit: Option[Int] = None
  ): Seq[AttributeList] = {

    val association = makeCFindAssociationForImagesInSeries(callingAETitle: String, calledPacs: PACS)
    val serUidAttr = AttributeFactory.newAttribute(TagByName.SeriesInstanceUID)
    serUidAttr.addValue(SeriesInstanceUID)
    val newQuery = DicomUtil.clone(query)
    newQuery.put(serUidAttr)
    val list = cFind2(association, newQuery, queryLevel = QueryRetrieveLevel.IMAGE, limit)
    if (association != null)
      try {
        Trace.trace()
        association.abort()
        Trace.trace()
      } catch {
        case t: Throwable =>
          logger.warn(s"Unexpected exception while aborting C-FIND DICOM association.   Query: ${query.toString().replaceAll("\n", "  ||  ")} : ${fmtEx(t)}")
          Trace.trace()
      }

    list
  }

  private def cFindSeriesInPatient(
      callingAETitle: String,
      calledPacs: PACS,
      PatientID: String,
      query: AttributeList = makeCFindQueryForSeriesInPatient(),
      limit: Option[Int] = None
  ): Seq[AttributeList] = {

    val association = makeCFindAssociationForSeriesInPatient(callingAETitle: String, calledPacs: PACS)
    val serUidAttr = AttributeFactory.newAttribute(TagByName.PatientID)
    serUidAttr.addValue(PatientID)
    val newQuery = DicomUtil.clone(query)
    newQuery.put(serUidAttr)
    val list = cFind2(association, newQuery, queryLevel = QueryRetrieveLevel.SERIES, limit)
    if (association != null)
      try {
        Trace.trace()
        association.abort()
        Trace.trace()
      } catch {
        case t: Throwable =>
          logger.warn(s"Unexpected exception while aborting C-FIND DICOM association for series in patient.   Query: ${query.toString().replaceAll("\n", "  ||  ")} : ${fmtEx(t)}")
          Trace.trace()
      }

    list
  }

  def main(args: Array[String]): Unit = {

    val callingAETitle = "IRRER"
    val calledPacs = new PACS("VMSDBD", "10.30.65.100", 105)

    if (false) {
      val start = System.currentTimeMillis()
      val list = cFindImagesInSeries(callingAETitle, calledPacs, "1.2.246.352.62.2.4716894529655432042.6463001790075780501")
      val elapsed = System.currentTimeMillis() - start
      println(list.mkString("\n\n"))
      println(s"Get the list of images of a series with cFindImagesInSeries.  slices: ${list.size}   Elapsed ms: $elapsed")
      println(list.mkString("\n\n"))
      println(s"Get the list of images of a series with cFindImagesInSeries.  slices: ${list.size}   Elapsed ms: $elapsed")
    }

    def doImageQuery(query: AttributeList, testName: String): Unit = {
      val association = makeCFindAssociationForImagesInSeries(callingAETitle, calledPacs)

      val serUidAttr = AttributeFactory.newAttribute(TagByName.SeriesInstanceUID)
      serUidAttr.addValue("1.2.246.352.62.2.4716894529655432042.6463001790075780501")
      query.put(serUidAttr)

      println(s"$testName query: " + DicomUtil.attributeListToString(query))

      val start = System.currentTimeMillis()
      for (i <- 0 until 100) {
        val list = cFind2(association, query, queryLevel = QueryRetrieveLevel.IMAGE, limit = None)
        if (i == 0)
          println(s"$testName Find number: $i    list.size: ${list.size}    first list item:\n${DicomUtil.attributeListToString(list.head)}")
        else
          print(s" $i")
      }
      println("\n")

      val elapsed = System.currentTimeMillis() - start
      println(s"$testName Get the list of images of a series with cFind2.  Elapsed ms: $elapsed")

      val startAbort = System.currentTimeMillis()
      association.abort()
      val elapsedAbort = System.currentTimeMillis() - startAbort
      println(s"$testName Done.  Elapsed ms for association abort: $elapsedAbort")
    }

    if (false) {
      val query = makeMinimalCFindQueryForImagesInSeries(Some("1.2.246.352.62.2.4716894529655432042.6463001790075780501"))
      doImageQuery(query, "Minimal Query")
    }

    if (false) {
      val query = makeCFindQueryForImagesInSeries(Some("1.2.246.352.62.2.4716894529655432042.6463001790075780501"))
      doImageQuery(query, "Full Query")
    }

    if (true) {
      val start = System.currentTimeMillis()
      val list = cFindSeriesInPatient(callingAETitle, calledPacs, "$AQA_TB3")
      val elapsed = System.currentTimeMillis() - start
      println(list.mkString("\n\n"))
      println(s"Get the list of series for a patient using cFindSeriesInPatient.  slices: ${list.size}   Elapsed ms: $elapsed")
      println(list.mkString("\n\n"))
      println(s"Get the list of series for a patient using cFindSeriesInPatient.  slices: ${list.size}   Elapsed ms: $elapsed")

    }

  }

}
