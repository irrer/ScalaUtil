package test.java

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace
import edu.umro.util.UMROGUID

import java.io.File

object Sectra {

  private def appName = "FixSectra"

  private val dir = new File("""D:\tmp\scott\AAE0A8D0\256\0_124D""")
  private val outDir = new File("""D:\tmp\scott\AAE0A8D0\256\Fixed\MultiSeries""")
  private def readFile(file: File): AttributeList = {
    val al = new AttributeList
    al.read(file)
    al
  }

  private def fix(al: AttributeList, instanceNumber: Int): Unit = {

    def newUid(tag: AttributeTag): Unit = {
      val at = al.get(tag)
      at.removeValues()
      at.addValue(UMROGUID.getUID)
    }

    def newIN(tag: AttributeTag): Unit = {
      val at = al.get(tag)
      at.removeValues()
      at.addValue(instanceNumber)
    }

    def newPId(tag: AttributeTag): Unit = {
      val at = al.get(tag)
      at.removeValues()
      at.addValue("30661001")
    }

    newUid(TagByName.SeriesInstanceUID)
    newUid(TagByName.SeriesInstanceUID)
    newIN(TagByName.InstanceNumber)
    newPId(TagByName.PatientID)

    val outFile = new File(outDir, "Sectra" + "%02d".format(instanceNumber) + ".dcm")

    Trace.trace(outFile.getAbsolutePath)
    // Trace.trace(DicomUtil.attributeListToString(al))

    DicomUtil.writeAttributeListToFile(al, outFile, appName)
  }

  def main(args: Array[String]): Unit = {

    val alList = FileUtil.listFiles(dir).map(readFile).sortBy(al => DicomUtil.getTimeAndDate(al, TagByName.InstanceCreationDate, TagByName.InstanceCreationTime).get.getTime)

    alList.zipWithIndex.foreach(ai => fix(ai._1, ai._2 + 1))
  }

}
