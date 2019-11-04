
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

class TestDicomUtil_TreatmentMachineType extends FlatSpec with Matchers {

  val dir = new File("""src\test\resources\TreatmentMachineType""")

  "good" should "be good" in {

    def nameMatches(fileName: String): Option[DicomUtil.TreatmentMachineType.Value] = {
      val all = DicomUtil.TreatmentMachineType.values
      val expectedName = fileName.toLowerCase.replaceAll(".dcm$", "")
      all.find(tmt => tmt.toString.toLowerCase.equals(expectedName))
    }

    def testRtplan(f: File) = {
      val al = new AttributeList
      al.read(f)
      val tmt = DicomUtil.TreatmentMachineType.attrListToTreatmentMachineType(al)
      val expected = nameMatches(f.getName)
      println("tmt: " + tmt)
      println("expected: " + expected)

      val ok = (tmt, expected) match {
        case (None, None) => true
        case (Some(t), Some(e)) => t.toString.equals(e.toString)
        case _ => false
      }

      println("testing: " + tmt + " == " + expected)
      (ok) should be(true)
    }

    dir.listFiles.map(f => testRtplan(f))
  }
}
