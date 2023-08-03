package test.java

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import edu.umro.util.UMROGUID

import java.io.File

object SectraFix {

  private def appName = "FixSectra"

  private val dir = new File("""D:\tmp\scott\original\0000D8BF""")
  private val outDir = new File("""D:\tmp\scott\fixed\0000D8BF""")

  private val PhaseNumberTag = TagByName.PhaseNumber

  private def readFile(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  private def getPhaseNumberAttr(al: AttributeList): Attribute = al.get(PhaseNumberTag)

  private def getPhaseNumber(al: AttributeList): Int = getPhaseNumberAttr(al).getIntegerValues.head

  private def fixSlice(al: AttributeList, SeriesInstanceUID: String): Unit = {

    def setAttrValue(at: Attribute, value: String): Unit = {
      at.removeValues()
      at.addValue(value)
    }

    setAttrValue(al.get(TagByName.SOPInstanceUID), UMROGUID.getUID)
    setAttrValue(al.get(TagByName.SeriesInstanceUID), SeriesInstanceUID)
    setAttrValue(al.get(TagByName.PatientID), "30661001")

    val outFile = {
      val InstanceNumber = al.get(TagByName.InstanceNumber).getIntegerValues.head

      val PhaseNumberText = "%02d".format(getPhaseNumber(al))
      val InstanceNumberText = "%03d".format(InstanceNumber)

      val name = s"Sectra201-$PhaseNumberText-$InstanceNumberText.dcm"
      new File(outDir, name)
    }

    println(s"Writing file ${outFile.getAbsolutePath}")

    DicomUtil.writeAttributeListToFile(al, outFile, appName)
  }

  private def fixGroup(alList: Seq[AttributeList]): Unit = {
    val SeriesInstanceUID = UMROGUID.getUID
    alList.foreach(al => fixSlice(al, SeriesInstanceUID))
  }

  def main(args: Array[String]): Unit = {

    Trace.trace("Starting ...")

    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs()
    println("Old output directory deleted.")

    val start = System.currentTimeMillis()

    println("Reading files...")
    val alList = FileUtil.listFiles(dir).map(readFile).groupBy(getPhaseNumber).values

    println("Fixing files...")
    alList.foreach(fixGroup)

    val elapsed = System.currentTimeMillis() - start
    Trace.trace(s"Done. Elapsed ms: $elapsed")

  }

}
