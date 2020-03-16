package edu.umro.ScalaUtil

import java.io.File
import edu.umro.util.Utility
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TransferSyntax
import java.util.Date
import com.pixelmed.dicom.OtherByteAttribute
import com.pixelmed.dicom.OtherWordAttribute
import java.io.FileOutputStream
import com.pixelmed.dicom.FileMetaInformation
import com.pixelmed.dicom.OtherByteAttributeOnDisk
import com.pixelmed.dicom.Attribute
import java.text.SimpleDateFormat
import org.scalatest.Fact.IsEqvTo
import java.util.Calendar
import java.util.TimeZone
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy

object RedoDicomImport extends Logging {
  private def sopOf(al: AttributeList) = new String(al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString)

  private def readFile(file: File) = {
    val al = new AttributeList
    //Trace.trace("reading file: " + file.getAbsolutePath)
    al.read(file)
    al
  }

  private case class DF(file: File) {
    val sop = sopOf(readFile(file))
  }

  /**
   * Safely get a list of files in a directory.  On failure, return an empty list.
   */
  private def listFiles(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    } catch {
      case t: Throwable => List[File]()
    }
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    try {

      val expDirList = listFiles(new File("""D:\tmp\mig\redone"""))
      val expFileList = expDirList.map(d => listFiles(d)).flatten
      val sopList = expFileList.map(f => new DF(f))
      Trace.trace("Number of exported files: " + sopList.size)

      val inboxSopList = listFiles(new File("""D:\tmp\mig\redo""")).map(f => new DF(f))
      Trace.trace("Number of files in inbox: " + inboxSopList.size)

      inboxSopList.map(in => {
        sopList.find(df => df.sop.equals(in.sop)) match {
          case Some(m) => println("ls " + in.file.getName + " ../redone/" + m.file.getParentFile.getName + "/" + m.file.getName)
          case _ => println("Failed: " + in.file.getName)
        }
      })

      Trace.trace("Elapsed ms: " + (System.currentTimeMillis - start))
    } catch {
      case t: Throwable => {
        Trace.trace("Elapsed ms: " + (System.currentTimeMillis - start) + "    Unexpected exception: " + fmtEx(t))
      }
    }
  }

}
