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

package edu.umro.ScalaUtil.DicomSort

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil

object FORMap {

  /**
   * List of FORs : Frame of Reference UIDs.  Each is assigned an integer to be used
   * as an abbreviation for the user so they don't have to deal with DICOM UIDs.
   *
   * [ FrameOfReferenceUID, KeyUniqueToFrameOfReferenceUID ]
   */
  private val forList = scala.collection.mutable.Map[String, Int]()

  /**
   * Get the frames of references as a list of indexes.
   *
   * @param al Contains frames of reference.
   * @return List of indexes.
   */
  def getForList(al: AttributeList): Seq[Int] = {
    val list = DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).map(attr => attr.getSingleStringValueOrEmptyString).distinct

    def addToList(forUID: String): Int = {
      if (!forList.contains(forUID)) {
        val index = forList.size
        forList.put(forUID, index)
      }
      forList(forUID)
    }

    val j = list.map(forUID => addToList(forUID))
    val j1 = al.get(TagFromName.Modality).getSingleStringValueOrEmptyString
    list.map(forUID => addToList(forUID))
  }


}
