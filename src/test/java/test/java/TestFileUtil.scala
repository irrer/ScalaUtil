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
import java.io.FileInputStream

/**
 * Test the file utilities.
 *
 */

// TODO needs much work
class TestFileUtil extends FlatSpec with Matchers {

  "read zipped input stream" should "make a list of named byte arrays" in {

    val inList = new File("""src\test\resources\ZipToNamedArrays""").listFiles

    def doit(file: File) {
      val array = FileUtil.readBinaryFile(file).right.get
      val fis = new FileInputStream(file)
      val content = FileUtil.writeZipToNamedByteArrays(fis)
      content.map(nba => println(nba._1 + " size: " + nba._2.size))
    }

    inList.map(doit)

    if (true) {
      Trace.trace
      val file = new File("""D:\pf\eclipse\workspaceOxygen\ScalaUtil\src\test\resources\ZipToNamedArrays\DICOM_1_.1552409651073.67.zip""")
      Trace.trace("file: " + file.getAbsolutePath)
      Trace.trace
      Trace.trace
      val content = FileUtil.readBinaryFile(file).right.get
      Trace.trace("got old content")
      val oldAlList = edu.umro.ScalaUtil.DicomUtil.zippedByteArrayToDicom(content)
      Trace.trace("Got old content.  size: " + oldAlList.size)
      Trace.trace
    }

    true should be(true)
  }
}
