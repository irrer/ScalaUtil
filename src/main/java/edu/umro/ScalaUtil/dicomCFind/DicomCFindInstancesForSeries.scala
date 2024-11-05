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

package edu.umro.ScalaUtil.dicomCFind

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TagFromName
import com.pixelmed.network.IdentifierHandler
import edu.umro.ScalaUtil.DicomCliUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PACS

/**
  * List all instances (e.g. slices or images) for a given series.
  */

class DicomCFindInstancesForSeries(callingAETitle: String, calledPacs: PACS, retrieveList: Seq[AttributeTag] = DicomCFindInstancesForSeries.defaultRetrieveList)
    extends DicomCFindBase(callingAETitle, calledPacs, retrieveList) {

  override protected val queryRetrieveInformationModel: String = SOPClass.StudyRootQueryRetrieveInformationModelFind
  override protected val queryRetrieveLevel: String = "IMAGE"

  /**
    * Simplified version tailored to use SeriesInstanceUID
    * @param SeriesInstanceUID For this series.
    * @return List of series.
    */
  def findBySeriesInstanceUID(SeriesInstanceUID: String): Seq[AttributeList] = {
    val al = new AttributeList

    val serUidAttr = AttributeFactory.newAttribute(TagFromName.SeriesInstanceUID)
    serUidAttr.addValue(SeriesInstanceUID)
    al.put(serUidAttr)

    super.find(al)
  }

}

private object DicomCFindInstancesForSeries extends IdentifierHandler with Logging {

  /**
    * Default list of tags to retrieve.  The called PACS may or may not return these, depending on
    * whether it supports it.  See the conformance statement for the PACS for details.
    */
  private val defaultRetrieveList: Seq[AttributeTag] = Seq(
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
    TagFromName.SeriesDate,
    TagFromName.SeriesTime
  )

  // ----------------------------------------------------------------------------------------------------

  def main(args: Array[String]): Unit = {
    import DicomCliUtil._
    import org.apache.commons.cli.Options

    val options = new Options()

    addClientAETitleOption(options)
    addServerOptions(options)

    val commandLine = parseOptions(options, args)

    if (commandLine.isEmpty)
      System.exit(1)
    else {
      val cl = commandLine.get

      val cFind = new DicomCFindInstancesForSeries(getClientAETitle(cl), getServerPACS(cl))

      def findSeriesData(SeriesInstanceUID: String): Unit = {
        val result = cFind.findBySeriesInstanceUID(SeriesInstanceUID)
        val text = findResultToText(result)
        println("\n")
        println(s"SeriesInstanceUID: $SeriesInstanceUID    Number of results: ${result.size}\n$text")
      }

      val seriesUidList = cl.getArgs

      if (seriesUidList.isEmpty)
        showHelp(options)
      else
        seriesUidList.foreach(findSeriesData)

    }
  }

}
