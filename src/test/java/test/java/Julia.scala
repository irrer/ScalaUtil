package test.java

import java.io.File
import edu.umro.util.Utility
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TransferSyntax
import java.util.Date
import com.pixelmed.dicom.OtherByteAttribute
import com.pixelmed.dicom.OtherWordAttribute
import java.io.FileOutputStream
import com.pixelmed.dicom.FileMetaInformation
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.OtherByteAttributeOnDisk
import com.pixelmed.dicom.Attribute
import edu.umro.ScalaUtil.FileUtil

object Julia {

  private val outDir = new File("hey")
  private val frameOfRefSet = scala.collection.mutable.Set[String]()

  private val doneSet = scala.collection.mutable.Set[String]()

  private val uidMap = scala.collection.mutable.HashMap[String, String]()

  private def getNewUid(oldUid: String): String = {
    if (uidMap.contains(oldUid)) uidMap(oldUid)
    else {
      val newUid = UMROGUID.getUID
      uidMap.put(oldUid, newUid)
      newUid
    }
  }

  private def updateAttrUid(attr: Attribute) = {
    val oldUid = attr.getSingleStringValueOrEmptyString
    val newUid = getNewUid(oldUid)
    attr.removeValues
    attr.addValue(newUid)
  }

  val transferSyntax = TransferSyntax.ImplicitVRLittleEndian

  private def readFile(file: File) = {
    val al = new AttributeList
    //println("reading file: " + file.getAbsolutePath)
    al.read(file)
    al
  }

  private def writeFile(al: AttributeList, file: File) = {
    FileMetaInformation.addFileMetaInformation(al, transferSyntax, "JimIrrer")
    DicomUtil.writeAttributeListToFile(al, file, "JimIrrer")
    println("Created " + file.getAbsolutePath)
  }

  private def seriesOf(al: AttributeList) = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

  private def zPosOf(al: AttributeList) = {
    val ipp = al.get(TagFromName.ImagePositionPatient)
    if (ipp == null) 0 else ipp.getDoubleValues()(2)
  }

  private def modalityOf(al: AttributeList) = al.get(TagFromName.Modality).getSingleStringValueOrEmptyString

  private def sopOf(al: AttributeList) = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString

  private def seriesUidOf(al: AttributeList) = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

  private def frameOfRefOf(al: AttributeList): String = {
    val list = DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).map(at => at.getSingleStringValueOrEmptyString)

    if (modalityOf(al).equals("REG") && (list.size > 1)) {
      val mainFrmOfRef = al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
      list.filterNot(frmOfRef => frmOfRef.eq(mainFrmOfRef)).head
    } else {
      if (list.isEmpty)
        "none"
      else
        list.head
    }
  }

  private def refPlanOf(al: AttributeList): String = {
    if (al.get(TagFromName.ReferencedRTPlanSequence) != null) {
      val refSeq = DicomUtil.seqToAttr(al, TagFromName.ReferencedRTPlanSequence).head.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString
      refSeq
    } else ""
  }

  private class DcmFl(f: File, al: AttributeList) {
    val file = f
    val modality = new String(modalityOf(al))
    val sop = new String(sopOf(al))
    val seriesUid = new String(seriesUidOf(al))
    val frameOfRef = new String(frameOfRefOf(al))
    val referencedPlan = new String(refPlanOf(al))
    val zPos = zPosOf(al) + 0

    def copyTo(dest: File) = {
      val data = FileUtil.readBinaryFile(file).right.get
      FileUtil.writeBinaryFile(dest, data)
    }
  }

  private def fixRtstruct(rtstructDF: DcmFl, outDir: File, frameOfRefIndex: Int, rtstructIndex: Int, ctList: Seq[DcmFl]) = {
    val rtstruct = readFile(rtstructDF.file)
    val refFrmOfRef = DicomUtil.seqToAttr(rtstruct, TagFromName.ReferencedFrameOfReferenceSequence).head
    val studySeq = DicomUtil.seqToAttr(refFrmOfRef, TagFromName.RTReferencedStudySequence).head
    val seriesSeq = DicomUtil.seqToAttr(studySeq, TagFromName.RTReferencedSeriesSequence).head
    val contourSeq = DicomUtil.seqToAttr(seriesSeq, TagFromName.ContourImageSequence)

    val imageSeriesUid = ctList.head.seriesUid
    val refSeries = seriesSeq.get(TagFromName.SeriesInstanceUID)
    refSeries.removeValues
    refSeries.addValue(imageSeriesUid)

    def fixContour(al: AttributeList, uid: String) = {
      val refSop = al.get(TagFromName.ReferencedSOPInstanceUID)
      refSop.removeValues
      refSop.addValue(uid)
    }

    contourSeq.zip(ctList.map(ct => ct.sop)).map(contourUid => fixContour(contourUid._1, contourUid._2))

    val file = new File(outDir, "RTSTRUCT_" + fmt(frameOfRefIndex) + "_" + fmt(rtstructIndex) + ".dcm")
    writeFile(rtstruct, file)
    Trace.trace("wrote file " + file.getAbsolutePath)
  }

  def fmt(i: Int) = i.formatted("%03d")

  private def saveCtList(ctList: Seq[DcmFl], index: Int, outDir: File) = {
    val ctDir = new File(outDir, "CT_" + fmt(index))
    ctDir.mkdirs
    ctList.zipWithIndex.map(ctIdx => {
      val dest = new File(ctDir, "CT-" + fmt(ctIdx._2 + 1))
      ctIdx._1.copyTo(dest)
    })
  }

  private def saveRtplan(rtplanDM: DcmFl, outDir: File, frmOfRefIndex: Int, planIndex: Int) = {
    val dest = new File(outDir, "RTPLAN_" + fmt(frmOfRefIndex) + "-" + fmt(planIndex) + ".dcm")
    rtplanDM.copyTo(dest)
  }

  private def saveRtimage(rtimageDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtimageIndex: Int) = {
    val dest = new File(outDir, "RTIMAGE_" + fmt(frmOfRefIndex) + "-" + fmt(rtimageIndex) + ".dcm")
    rtimageDM.copyTo(dest)
  }

  private def saveReg(regDM: DcmFl, outDir: File, frmOfRefIndex: Int, regIndex: Int) = {
    val dest = new File(outDir, "REG_" + fmt(frmOfRefIndex) + "-" + fmt(regIndex) + ".dcm")
    regDM.copyTo(dest)
  }

  private def saveRtdose(rtdoseDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtdoseIndex: Int) = {
    val dest = new File(outDir, "RTDOSE_" + fmt(frmOfRefIndex) + "-" + fmt(rtdoseIndex) + ".dcm")
    rtdoseDM.copyTo(dest)
  }

  private def saveRtrecord(regDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtrecordIndex: Int) = {
    val dest = new File(outDir, "RTRECORD_" + fmt(frmOfRefIndex) + "-" + fmt(rtrecordIndex) + ".dcm")
    regDM.copyTo(dest)
  }

  private def getByRefPlan(frmOfRef: String, rtplanList: Seq[DcmFl], allDcm: Seq[DcmFl]): Seq[DcmFl] = {
    val rtplanSopList = rtplanList.map(rtplan => rtplan.sop)
    allDcm.filter(d => rtplanSopList.contains(d.referencedPlan))
  }

  private def fix(frmOfRef: String, frmOfRefIndex: Int, allDcm: Seq[DcmFl], outDir: File) = {
    val frmOfRefDir = new File(outDir, "FrmofRef_" + fmt(frmOfRefIndex))
    val frmOfRefList = allDcm.filter(d => d.frameOfRef.equals(frmOfRef))
    val ctList = frmOfRefList.filter(d => d.modality.equals("CT")).sortBy(d => d.zPos)
    saveCtList(ctList, frmOfRefIndex, frmOfRefDir)

    val rtstructList = frmOfRefList.filter(d => d.modality.equals("RTSTRUCT"))
    // private def fixRtstruct(rtstructDF: DcmFl, outDir : File, index: Int, frameOfRefIndex: Int, rtstructIndex: Int, ctList: Seq[DcmFl]) = {
    rtstructList.zipWithIndex.map(di => fixRtstruct(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1, ctList))

    val rtplanList = allDcm.filter(d => d.frameOfRef.equals(frmOfRef) && d.modality.equals("RTPLAN"))
    // private def fixRtstruct(rtstructDF: DcmFl, outDir : File, index: Int, frameOfRefIndex: Int, rtstructIndex: Int, ctList: Seq[DcmFl]) = {
    rtplanList.zipWithIndex.map(di => saveRtplan(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))

    val rtimageList = frmOfRefList.filter(d => d.modality.equals("RTIMAGE"))
    rtimageList.zipWithIndex.map(di => saveRtimage(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))

    val regList = frmOfRefList.filter(d => d.modality.equals("REG"))
    regList.zipWithIndex.map(di => saveReg(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))

    val refRtplanList = getByRefPlan(frmOfRef, rtplanList, allDcm).filter(d => d.modality.equals("RTRECORD") || d.modality.equals("RTDOSE"))

    val rtdoseList = refRtplanList.filter(d => d.modality.equals("RTDOSE"))
    rtdoseList.zipWithIndex.map(di => saveRtdose(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))

    val rtrecordList = refRtplanList.filter(d => d.modality.equals("RTRECORD"))
    rtrecordList.zipWithIndex.map(di => saveRtrecord(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))
  }

  private def saveOther(allDcm: Seq[DcmFl], outDir: File) = {
    val noFrameOfRef = allDcm.filter(d => d.frameOfRef.size <= 5)
    val byModality = allDcm.filter(d =>
      (!d.modality.equals("CT")) &&
        (!d.modality.equals("RTSTRUCT")) &&
        (!d.modality.equals("RTIMAGE")) &&
        (!d.modality.equals("REG")) &&
        (!d.modality.equals("RTRECORD")) &&
        (!d.modality.equals("RTDOSE")) &&
        (!d.modality.equals("RTPLAN")))

    def saveSeries(series: Seq[DcmFl], seriesIndex: Int) = {
      if (series.size == 1) {
        val dest = new File(outDir, series.head.modality + "-" + fmt(seriesIndex) + ".dcm")
        series.head.copyTo(dest)
      } else {
        val seriesDir = new File(outDir, series.head.modality + "-" + fmt(seriesIndex))
        seriesDir.mkdirs
        series.zipWithIndex.map(di => {
          val dest = new File(seriesDir, series.head.modality + fmt(di._2 + 1) + ".dcm")
          di._1.copyTo(dest)
        })
      }
    }

    (noFrameOfRef ++ byModality).
      groupBy(_.seriesUid).
      map(ss => ss._2).
      zipWithIndex.map(ssi => saveSeries(ssi._1, ssi._2 + 1))
  }

  /**
   * Safely get a list of files in a directory.  On failure, return an empty list.
   */
  def listFiles(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    } catch {
      case t: Throwable => List[File]()
    }
  }

  private def listRegularFiles(file: File): Seq[File] = {
    if (file.isFile) Seq(file)
    else {
      val fList = listFiles(file)
      fList.map(f => listRegularFiles(f)).flatten
    }
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    val mainDir = new File("""D:\tmp\julia\DUST1""") //      new File(args.head)
    val outDir = new File(mainDir.getParentFile, mainDir.getName + "fixed")
    Utility.deleteFileTree(outDir)
    outDir.mkdirs

    val allFiles = listRegularFiles(mainDir).filter(f => f.getName.toLowerCase.endsWith(".dcm"))
    Trace.trace("allFiles.size: " + allFiles.size)

    // read meta data and remove duplicate SOPs
    val allDcm = allFiles.map(f => new DcmFl(f, readFile(f))).groupBy(_.sop).map(_._2.head).toSeq

    val frmOfRefList = allDcm.map(d => d.frameOfRef).distinct

    frmOfRefList.zipWithIndex.map(frmOfRefIdx => fix(frmOfRefIdx._1, frmOfRefIdx._2 + 1, allDcm, outDir))

    //saveOther(allDcm, outDir)

    println(mainDir.getName + "  Elapsed ms: " + (System.currentTimeMillis - start))
  }

}