/*
 * Copyright 2024 Regents of the University of Michigan
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

package test.java.dicomCMove

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PACS
import edu.umro.ScalaUtil.dicomCMove.DicomCMoveGetter
import edu.umro.ScalaUtil.dicomCMove.DicomCMoveReceiver

import java.io.File

/**
  * Test receiving files from a remote PACS.
  */
object DicomCMoveReceiverTestLocalConquest extends Logging {

  def main(args: Array[String]): Unit = {

    logger.info("Starting")
    val mainDir = new File("""D:\tmp\getter\test""")
    val srcPacs = new PACS("IRRER", "127.0.0.1", 15678)
    val thisPacs = new PACS("WLQA_TEST", "141.214.125.209", 5682)

    val rec = DicomCMoveReceiver(mainDir, thisPacs)

    val getter = new DicomCMoveGetter(srcPacs, rec)

    if (true) {
      val result = getter.getInstance("1.3.6.1.4.1.22361.17483843774714.348180522.1652902828325.26")
      result.errorMessage match {
        case Some(message) => println(s"Failure: $message")
        case _ =>
          val size = FileUtil.listFiles(result.dir).size
          println(s"getInstance Received $size files into ${result.dir.getAbsolutePath}")
      }
    }

    if (true) {
      val result = getter.getSeries("1.3.6.1.4.1.22361.17483843774714.348180522.1652902828325.23")
      result.errorMessage match {
        case Some(message) => println(s"Failure: $message")
        case _ =>
          val size = FileUtil.listFiles(result.dir).size
          println(s"getSeries Received $size files into ${result.dir.getAbsolutePath}")
      }
    }

    if (true) {
      val result = getter.getPatient("$AQA_49")
      result.errorMessage match {
        case Some(message) => println(s"Failure: $message")
        case _ =>
          val size = FileUtil.listFiles(result.dir).size
          println(s"getPatient Received $size files into ${result.dir.getAbsolutePath}")
      }
    }

    if (true) {
      logger.info("shutting down")
      rec.close()

      println("Failure expected after shutting down receiver.")

      val result = getter.getInstance("1.2.246.352.62.1.5447425699951255303.4394006998145014936")
      result.errorMessage match {
        case Some(message) => println(s"Failure: $message")
        case _ =>
          val size = FileUtil.listFiles(result.dir).size
          println(s"getInstance Received $size files into ${result.dir.getAbsolutePath}")
      }
    }

    Thread.sleep(1000)
    logger.info("Done.  Exiting.")
    System.exit(0)
  }
}
