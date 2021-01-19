package edu.umro.ScalaUtil.DicomTree

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
