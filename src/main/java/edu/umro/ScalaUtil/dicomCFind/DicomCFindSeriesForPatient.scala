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
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PACS

/**
  * List series for a patient.
  */

class DicomCFindSeriesForPatient(callingAETitle: String, calledPacs: PACS, retrieveList: Seq[AttributeTag] = DicomCFindSeriesForPatient.defaultRetrieveList)
    extends DicomCFindBase(callingAETitle, calledPacs, retrieveList) {

  override protected val queryRetrieveInformationModel: String = SOPClass.StudyRootQueryRetrieveInformationModelFind
  override protected val queryRetrieveLevel: String = "SERIES"

  /**
    * Simplified version tailored to use SeriesInstanceUID
    * @param PatientID Only get series for this patient.  This may optionally contain wildcard characters, e.g. 1005*23
    * @return List of series.
    */
  def findSeriesForPatient(PatientID: String, Modality: Option[String] = None): Seq[AttributeList] = {
    val al = new AttributeList

    val serUidAttr = AttributeFactory.newAttribute(TagFromName.PatientID)
    serUidAttr.addValue(PatientID)
    al.put(serUidAttr)

    if (Modality.isDefined) {
      val attr = AttributeFactory.newAttribute(TagFromName.Modality)
      attr.addValue(Modality.get)
      al.put(attr)
    }

    super.find(al)
  }

}

object DicomCFindSeriesForPatient extends IdentifierHandler with Logging {

  /**
    * Default list of tags to retrieve.  The called PACS may or may not return these, depending on
    * whether it supports it.  See the conformance statement for the PACS for details.
    */
  private val defaultRetrieveList: Seq[AttributeTag] = Seq(
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

}
