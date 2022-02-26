package edu.umro.ScalaUtil.DicomSort

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil


/**
 * Insert a Varian integrity check in an RTPLAN.
 */
object VarianTreatmentPlanIntegrityCheck {

  def beamNumber(beam: AttributeList): Int = {
    beam.get(TagByName.BeamNumber).getIntegerValues.head
  }

  def main(args: Array[String]): Unit = {

    val rtplan = new AttributeList
    rtplan.read("rtplan.dcm")

    // list of beams, sorted by beam number
    val beamList = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence).sortBy(beamNumber)

  }

}
