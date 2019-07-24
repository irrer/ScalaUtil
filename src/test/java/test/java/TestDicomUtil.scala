
package aqa.test;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag

/**
 * Test DicomUtil.
 *
 */

class TestDicomUtil extends FlatSpec with Matchers {

  val file = new File("""src\test\resources\rtplan.dcm""")
  println("Using DICOM file " + file.getAbsolutePath)
  val source = new AttributeList
  source.read(file)

  "good" should "be good" in {

    val tagSet = Set(
      TagFromName.RTBeamLimitingDeviceType,
      TagFromName.PatientID,
      TagFromName.AbortFlag,
      TagFromName.CumulativeDoseReferenceCoefficient)

    val attrList = DicomUtil.findAll(source, tagSet)

    def sizeOf(tag: AttributeTag) = attrList.filter(at => at.getTag.compareTo(tag) == 0).size

    //println("attrList:\n" + attrList.mkString("\n").replace('\0', ' '))

    println("Total number of tags found: " + attrList.size)

    attrList.size should be(432)
    sizeOf(TagFromName.RTBeamLimitingDeviceType) should be(253)
    sizeOf(TagFromName.PatientID) should be(1)
    sizeOf(TagFromName.AbortFlag) should be(0)
    sizeOf(TagFromName.CumulativeDoseReferenceCoefficient) should be(178)
  }

  "roundtrip" should "get same data" in {
    Trace.trace

    val fileList = Seq(
      new File("""src\test\resources\rtplan.dcm"""),
      new File("""src\test\resources\vessel_a.dcm"""),
      new File("""src\test\resources\vessel_b.dcm"""))

    val alList = fileList.map(f => { val al = new AttributeList; al.read(f); al })

    Trace.trace
    val byteArray = DicomUtil.dicomToZippedByteArray(alList)
    Trace.trace
    val alRound = DicomUtil.zippedByteArrayToDicom(byteArray)
    Trace.trace

    val sopBefore = alList.map(al => al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString).mkString(" ")
    println("Round tripped DICOM size: " + alRound.size)
    println("Round tripped DICOM:\n" + alRound.head.toString.replace('\0', ' ').take(500))
    println("Round tripped DICOM size: " + alRound.size)
    println("SOPInstanceUID list: " + sopBefore)
    val sopAfter = alRound.map(al => al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString).mkString(" ")
    sopBefore should be(sopAfter)
    Trace.trace

  }
}
