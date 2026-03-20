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

package test.java

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File

/**
  * Test the file utilities.
  *
  */

class TestDicomUtilFindAll extends FlatSpec with Matchers {

  "findAll" should "find exact list of attributes" in {

    val dicom = new File("src/test/resources/rtplan.dcm")
    val al = new AttributeList
    al.read(dicom)

    {
      val list = DicomUtil.findAll(al, attr => attr.getTag.equals(TagByName.BeamNumber))
      println(s"Found: ${list.size}  expected: 23")
      list.size should be(23)
    }

    {
      def interesting(attr: Attribute): Boolean = attr.getSingleStringValueOrEmptyString.contains("11900")
      val list = DicomUtil.findAll(al, interesting _)
      println(s"Found: ${list.size}  expected: 25")
      list.size should be(25)
    }

  }
}
