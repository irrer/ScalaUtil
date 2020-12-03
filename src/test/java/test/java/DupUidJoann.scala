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
import scala.xml.Attribute
import com.pixelmed.dicom.OtherWordAttribute
import java.io.FileOutputStream
import com.pixelmed.dicom.FileMetaInformation
import edu.umro.ScalaUtil.DicomUtil

object DupUidJoann {

  private val outDir = new File("""D:\tmp\joann\output""")
  Utility.deleteFileTree(outDir)
  outDir.mkdirs

  var seriesIndex = 0

  val transferSyntax = TransferSyntax.ImplicitVRLittleEndian

  val studyUid = UMROGUID.getUID

  private def readFile(file: File) = {
    val al = new AttributeList
    println("reading file: " + file.getAbsolutePath)
    al.read(file)
    al
  }

  private def readAllFiles(dir: File) = {
    dir.listFiles.map(f => readFile(f))
  }

  private def posOf(alA: AttributeList, alB: AttributeList): Boolean = {
    val a = alA.get(TagFromName.ImagePositionPatient).getDoubleValues
    val b = alB.get(TagFromName.ImagePositionPatient).getDoubleValues

    (a(0) < b(0)) || (a(1) < b(1)) || (a(2) < b(2))
  }

  private def getPrefix(series: Seq[AttributeList]) = {
    seriesIndex = seriesIndex + 1
    series.head.get(TagFromName.Modality).getSingleStringValueOrDefault("JP") + "_" + seriesIndex + "_"
  }

  private def writeFile(prefix: String, index: Int, seriesUid: String, al: AttributeList) = {
    val sopUid = UMROGUID.getUID

    val SOPInstanceUID = AttributeFactory.newAttribute(TagFromName.SOPInstanceUID)
    SOPInstanceUID.addValue(sopUid)
    al.put(SOPInstanceUID)

    val MediaStorageSOPInstanceUID = AttributeFactory.newAttribute(TagFromName.MediaStorageSOPInstanceUID)
    MediaStorageSOPInstanceUID.addValue(sopUid)
    al.put(MediaStorageSOPInstanceUID)

    val StudyInstanceUID = AttributeFactory.newAttribute(TagFromName.StudyInstanceUID)
    StudyInstanceUID.addValue(studyUid)
    al.put(StudyInstanceUID)

    val SeriesInstanceUID = AttributeFactory.newAttribute(TagFromName.SeriesInstanceUID)
    SeriesInstanceUID.addValue(seriesUid)
    al.put(SeriesInstanceUID)

    val fileName = prefix + index.formatted("%03d") + ".dcm"
    val file = new File(outDir, fileName)
    FileMetaInformation.addFileMetaInformation(al, transferSyntax, "irrer")

    DicomUtil.writeAttributeListToFile(al, file, "FixDicom")
    println("Created " + file.getAbsolutePath)
  }

  private def fixUp(series: Seq[AttributeList]) = {
    val prefix = getPrefix(series)
    val sorted = series.sortWith(posOf _)
    val seriesUid = UMROGUID.getUID

    sorted.indices.map(i => writeFile(prefix, i, seriesUid, sorted(i)))
  }

  private def seriesOf(al: AttributeList) = {
    al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString +
      al.get(TagFromName.PatientPosition).getSingleStringValueOrEmptyString
  }

  private def processDir(dir: File) = {
    println("processing dir: " + dir.getAbsolutePath)
    val alList = readAllFiles(dir)
    alList.groupBy(al => seriesOf(al)).map(s => fixUp(s._2))
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    val mainDir = new File("""\\rodicom1\data\$AZ_Calla""")
    mainDir.listFiles.map(dir => processDir(dir))

    println("Elapsed ms: " + (System.currentTimeMillis - start))
  }

}