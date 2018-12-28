
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

/**
 * Test DicomUtil.
 *
 */

class TestDicomUtil extends FlatSpec with Matchers {

  val source = new AttributeList
  source.read(new File("""src\test\resources\rtplan.dcm"""))

  "good" should "be good" in {

    val tagList = DicomUtil.getInstancesOfAttribute(source, TagFromName.RTBeamLimitingDeviceType)

    println("tagList: " + tagList.mkString("\n").replace('\0', ' '))

    true should be(true)
  }
}
