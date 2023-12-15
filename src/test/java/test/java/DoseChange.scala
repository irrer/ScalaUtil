package test.java

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.OtherWordAttribute
import com.pixelmed.dicom.OtherWordAttributeOnDisk
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace

import java.io.File

/**
  * Test code to see if Varian allows you to import an RTDOSE file with DoseType set to EFFECTIVE (usual is PHYSICAL)
  * with the dose changed.
  *
  * Turns out, it works, so yay!
  *
  * https://dicom.innolitics.com/ciods/rt-dose/rt-dose/30040004
  * https://dicom.nema.org/medical/Dicom/2016b/output/chtml/part03/sect_C.8.8.3.html
  */

object DoseChange {

  private def getPixels(al: AttributeList): Array[Short] = {

    val otherWordOnDisk = {
      val d = al.get(TagByName.PixelData)
      d.asInstanceOf[OtherWordAttributeOnDisk]
    }
    val pixShortArray = otherWordOnDisk.getShortValues
    println(s"size: ${pixShortArray.size}")

    pixShortArray
  }

  private def changeDose(data: Array[Short]): Array[Short] = {

    var count = 0

    def increase(s: Short): Short = {
      if (s < 16384) {
        count = count + 1
        (s + 10).toShort
      } else
        s
    }

    val bigger = data.map(increase)

    Trace.trace(s"Number of values: ${data.length}   number changed: $count")

    bigger
  }

  private def moveNonPixelData(old: AttributeList): AttributeList = {

    val newAl = new AttributeList

    // move all but pixel data
    def moveAttr(tag: AttributeTag): Unit = {
      if (tag.getGroup < 0x7f00)
        newAl.put(old.get(tag))
    }

    old.keySet().toArray().toList.foreach(x => moveAttr(x.asInstanceOf[AttributeTag]))
    newAl
  }

  def main(args: Array[String]): Unit = {
    val fileIn = new File("""D:\tmp\eqd2\anon\dose.dcm""")
    val fileOut = new File("""D:\tmp\eqd2\anon\dose2.dcm""")

    val oldAl = new AttributeList()
    oldAl.read(fileIn)

    val pixShortArray = getPixels(oldAl)

    val bigger = changeDose(pixShortArray)

    Trace.trace()

    val newAl = moveNonPixelData(oldAl)

    val otherWord = new OtherWordAttribute(TagByName.PixelData)
    otherWord.removeValues()
    otherWord.setValues(bigger)
    newAl.put(otherWord)

    DicomUtil.writeAttributeListToFile(newAl, fileOut, "ChangeDose")

    Trace.trace("Done.")
  }
}
