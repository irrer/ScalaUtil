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
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.FileUtil

object ForKar {

  val mainDir = new File("""D:\tmp\kim""")
  val inDir = new File(mainDir, "032_BrightS")
  val outDir = new File(mainDir, "032_BrightSfixed")

  private var rtstructIndex = 1

  val transferSyntax = TransferSyntax.ImplicitVRLittleEndian

  private def readFile(file: File) = {
    val al = new AttributeList
    println("reading file: " + file.getAbsolutePath)
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

  private def frameOfRef(al: AttributeList) = {
    DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).head.getSingleStringValueOrEmptyString
  }

  private def fixRtstruct(rtstruct: AttributeList, imageSeriesUid: String, uidList: Seq[String]) = {
    val refFrmOfRef = DicomUtil.seqToAttr(rtstruct, TagByName.ReferencedFrameOfReferenceSequence).head
    val studySeq = DicomUtil.seqToAttr(refFrmOfRef, TagByName.RTReferencedStudySequence).head
    val seriesSeq = DicomUtil.seqToAttr(studySeq, TagByName.RTReferencedSeriesSequence).head
    val contourSeq = DicomUtil.seqToAttr(seriesSeq, TagByName.ContourImageSequence)

    val refSeries = seriesSeq.get(TagFromName.SeriesInstanceUID)
    refSeries.removeValues
    refSeries.addValue(imageSeriesUid)

    def fixContour(al: AttributeList, uid: String) = {
      val refSop = al.get(TagFromName.ReferencedSOPInstanceUID)
      refSop.removeValues
      refSop.addValue(uid)
    }

    def fixSopInstanceUid = {
      val sop = rtstruct.get(TagFromName.SOPInstanceUID)
      sop.removeValues
      sop.addValue(UMROGUID.getUID)
    }

    fixSopInstanceUid
    contourSeq.zip(uidList).map(contourUid => fixContour(contourUid._1, contourUid._2))

    val file = new File(outDir, "RTSTRUCT_" + rtstructIndex + ".dcm")
    rtstructIndex = rtstructIndex + 1
    writeFile(rtstruct, file)
    Trace.trace("wrote file " + file.getAbsolutePath)
  }

  private def fixUp(frmRef: Seq[Seq[AttributeList]]): Unit = {

    val imageSeriesList = frmRef.filterNot(series => modalityOf(series.head).equals("RTSTRUCT"))
    if (imageSeriesList.size != 1) throw new RuntimeException("Should be exactly 1 image set in frame of reference but there are " + imageSeriesList.size)
    val uidList = imageSeriesList.head.map(al => sopOf(al))
    val imageSeriesUid = seriesUidOf(imageSeriesList.head.head)

    val rtstructList = frmRef.filter(series => modalityOf(series.head).equals("RTSTRUCT")).flatten

    rtstructList.map(rtstruct => fixRtstruct(rtstruct, imageSeriesUid, uidList))

  }

  private def copyImageFiles(dicomFiles: Seq[(File, AttributeList)]) = {
    dicomFiles.map(dAl => {
      if (!modalityOf(dAl._2).equals("RTSTRUCT")) {
        val data = FileUtil.readBinaryFile(dAl._1).right.get
        val file = new File(outDir, dAl._1.getName)
        FileUtil.writeBinaryFile(file, data)
      }
    })
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    Utility.deleteFileTree(outDir)
    outDir.mkdirs

    val dicomFiles = inDir.listFiles.toSeq.filter(f => f.getName.endsWith(".dcm")).map(f => (f, readFile(f)))
    copyImageFiles(dicomFiles)
    val alList = dicomFiles.map(fAl => fAl._2)

    val seriesList = alList.groupBy(al => seriesOf(al)).map(sal => sal._2.sortBy(al => zPosOf(al))).toSeq

    val frameOfRefList = seriesList.groupBy(s => frameOfRef(s.head)).map(ss => ss._2).toSeq

    frameOfRefList.map(frmRef => fixUp(frmRef))

    println("Elapsed ms: " + (System.currentTimeMillis - start))
  }

}