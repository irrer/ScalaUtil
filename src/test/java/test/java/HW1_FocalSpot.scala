package test.java

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import edu.umro.util.UMROGUID

import java.io.File
import java.util.Date

object HW1_FocalSpot {

  private val dir = new File("""/cygdrive/d/aqa/tmp/HW1_FocalSpot""".stripMargin)

  val hw1Phase2 = {
    val file = new File(dir, "Phase2.dcm")
    val al = new AttributeList
    al.read(file)
    al
  }

  private def getAttrValue(tag: AttributeTag): String = {
    DicomUtil.findAllSingle(hw1Phase2, tag).head.getSingleStringValueOrNull
  }

  val stdFocalSpot = {
    val file = new File(dir, "rtplanFocalSpotMillenium.dcm")
    val al = new AttributeList
    al.read(file)
    al
  }

  val hw1fs = DicomUtil.clone(stdFocalSpot)

  /**
    * Make new UIDs
    * @param hw1fs Put them here (mutable)
    */
  private def fixUIDs(hw1fs: AttributeList): Unit = {

    def fix(tag: AttributeTag): Unit = {
      val uid = UMROGUID.getUID

      def fixOne(attr: Attribute): Unit = {
        attr.removeValues()
        attr.addValue(uid)
      }

      DicomUtil.findAllSingle(hw1fs, tag).foreach(fixOne)
    }

    val uidTagList = Seq(
      TagByName.SOPInstanceUID,
      TagByName.ReferencedSOPInstanceUID,
      TagByName.StudyInstanceUID,
      TagByName.SeriesInstanceUID,
      TagByName.FrameOfReferenceUID,
      TagByName.DoseReferenceUID,
      TagByName.ReferencedPrimaryDoseRefUID
    )

    uidTagList.foreach(fix)
  }

  private def fixCommonTags(hw1fs: AttributeList): Unit = {

    def fixAttr(tag: AttributeTag, value: String): Unit = {
      def fix(attr: Attribute, value: String): Unit = {
        attr.removeValues()
        attr.addValue(value)
      }
      DicomUtil.findAllSingle(hw1fs, tag).foreach(attr => fix(attr, value))
    }

    def copyAttr(tag: AttributeTag): Unit = fixAttr(tag, getAttrValue(tag))

    val now = new Date

    val nowTime = DicomUtil.dicomTimeFormat.format(now)
    val nowDate = DicomUtil.dicomDateFormat.format(now)

    copyAttr(TagByName.ToleranceTableLabel)

    fixAttr(TagByName.InstanceCreationDate, nowDate)
    fixAttr(TagByName.StudyDate, nowDate)
    fixAttr(TagByName.RTPlanDate, nowDate)

    fixAttr(TagByName.InstanceCreationTime, nowTime)
    fixAttr(TagByName.StudyTime, nowTime)
    fixAttr(TagByName.RTPlanTime, nowTime)

    fixAttr(TagByName.PatientName, "$HW1FocalSpot")
    fixAttr(TagByName.PatientID, "$HW1FocalSpot")
    fixAttr(TagByName.RTPlanLabel, "FocalSpotDev")

    copyAttr(TagByName.TreatmentMachineName)
    copyAttr(TagByName.DeviceSerialNumber)

    copyAttr(TagByName.InstitutionName)
    copyAttr(TagByName.InstitutionalDepartmentName)
  }

  def main(args: Array[String]): Unit = {

    Trace.trace("Starting")




    fixUIDs(hw1fs)
    fixCommonTags(hw1fs)

    val outFile = new File(dir, "HW1_FocalSpot.dcm")
    outFile.delete()
    DicomUtil.writeAttributeListToFile(hw1fs, outFile, "HW1FocalSpot")

    Trace.trace("Done")

  }

}
