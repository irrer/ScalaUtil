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
  * List patients that match a pattern.
  */

class DicomCFindPatientList(callingAETitle: String, calledPacs: PACS, retrieveList: Seq[AttributeTag] = DicomCFindPatientList.defaultRetrieveList)
    extends DicomCFindBase(callingAETitle, calledPacs, retrieveList) {

  override protected val queryRetrieveInformationModel: String = SOPClass.PatientRootQueryRetrieveInformationModelFind
  override protected val queryRetrieveLevel: String = "PATIENT"

  /**
    * Simplified version tailored to use SeriesInstanceUID
    * @param PatientID Only get series for this patient.  This may optionally contain wildcard characters, e.g. 1005*23
    * @return List of series.
    */
  def findPatientList(PatientID: String): Seq[AttributeList] = {
    val al = new AttributeList

    val serUidAttr = AttributeFactory.newAttribute(TagFromName.PatientID)
    serUidAttr.addValue(PatientID)
    al.put(serUidAttr)

    super.find(al)
  }

}

object DicomCFindPatientList extends IdentifierHandler with Logging {

  /**
    * Default list of tags to retrieve.  The called PACS may or may not return these, depending on
    * whether it supports it.  See the conformance statement for the PACS for details.
    */
  private val defaultRetrieveList: Seq[AttributeTag] = Seq(
    TagFromName.InstitutionName,
    TagFromName.PatientName,
    TagFromName.PatientBirthDate,
    TagFromName.PatientBirthTime,
    TagFromName.OtherPatientIDs
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

      val cFind = new DicomCFindPatientList(getClientAETitle(cl), getServerPACS(cl))

      def findPatientData(PatientID: String): Unit = {
        val result = cFind.findPatientList(PatientID)
        val text = findResultToText(result)
        println("\n----------------------------------------------------------------------------------------------------")
        println(s"PatientID: $PatientID    Number of results: ${result.size}\n$text")
      }

      val patientIdList = cl.getArgs

      if (patientIdList.isEmpty)
        showHelp(options)
      else
        patientIdList.foreach(findPatientData)
    }

  }
}
