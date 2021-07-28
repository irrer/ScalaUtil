/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


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
