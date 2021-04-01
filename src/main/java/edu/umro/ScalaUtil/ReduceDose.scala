package edu.umro.ScalaUtil

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.util.UMROGUID

import java.io.File
import scala.collection.mutable.ArrayBuffer

/**
 * Reduce dose in RTPLAN for Dale Litzenberg.
 */
object ReduceDose extends Logging {

  private val logBuffer = ArrayBuffer[String]()

  private def log(msg: String): Unit = {
    logBuffer += msg
    println(msg)
  }


  private def readFile(file: File): Option[AttributeList] = {
    try {
      val al = new AttributeList
      // al.setDecompressPixelData(true)
      log("reading file: " + file.getAbsolutePath)
      al.read(file)
      Some(al)
    } catch {
      case t: Throwable =>
        println(fmtEx(t))
        None
    }
  }


  def main(args: Array[String]): Unit = {
    try {
      val start = System.currentTimeMillis
      println("Starting " + args.mkString(" "))

      val inFile = new File("""D:\tmp\litz23\$FLASH001\RP.1.2.246.352.71.5.924020296521.260790.20210326093150.dcm""")

      val al = readFile(inFile).get

      def putAll(tag: AttributeTag, value: String): Unit = {
        def put(attr: Attribute): Unit = {
          attr.removeValues()
          attr.addValue(value)
        }

        DicomUtil.findAllSingle(al, tag).foreach(at => put(at))
      }

      def putScale(tag: AttributeTag, factor: Double): Unit = {
        def put(attr: Attribute): Unit = {
          val old = attr.getDoubleValues.head
          attr.removeValues()
          attr.addValue(factor * old)
        }

        DicomUtil.findAllSingle(al, tag).foreach(at => put(at))
      }

      putAll(TagByName.SOPInstanceUID, UMROGUID.getUID)
      putAll(TagByName.MediaStorageSOPInstanceUID, UMROGUID.getUID)
      putAll(TagByName.DoseReferenceUID, UMROGUID.getUID)

      // putScale(TagByName.BeamDose, factor = 2 / 3.0)
      putScale(TagByName.BeamMeterset, factor = 2 / 3.0)

      val outFile = new File(inFile.getParentFile, inFile.getName.replaceAll(".dcm", "Meterset.dcm"))

      DicomUtil.writeAttributeListToFile(al, outFile, "ReduceDose")


      println("Elapsed ms: " + (System.currentTimeMillis - start))
      System.exit(0)
    } catch {
      case t: Throwable =>
        log("Unexpected error: " + t.getMessage)
    }

  }

}
