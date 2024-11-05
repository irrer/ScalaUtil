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

package edu.umro.ScalaUtil.dicomCMove

import com.pixelmed.network.ReceivedObjectHandler
import edu.umro.ScalaUtil.Logging

class DicomCMoveReceivedObjectHandler extends ReceivedObjectHandler with Logging {

  private var count: Int = 0

  def getCount: Int = count

  def resetCount: Unit = { count = 0 }

  override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String): Unit = {
    count = count + 1
    logger.info(s"Received DICOM file number $getCount from $callingAETitle    DICOM file: $fileName")
  }
}
