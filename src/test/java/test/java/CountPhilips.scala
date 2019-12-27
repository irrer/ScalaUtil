package test.java

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
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.OtherByteAttributeOnDisk
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy

/**
 * Count number of occurrences of files from Philips CT
 */
object CountPhilips {

  var fileCount = 0

  var sum = 0.toLong

  val logFile = new File("""D:\tmp\mig\loggy.txt""")
  logFile.delete

  logFile.createNewFile

  val logOut = new FileOutputStream(logFile)

  def loggy(msg: String) = {
    logOut.write((msg + "\n").getBytes)
  }

  private def readFile(file: File) = {
    val al = new AttributeList
    al.read(file)
    al
  }

  private def isPhilips(al: AttributeList) = {
    def is(tag: AttributeTag, value: String) = {
      (al.get(tag) != null) && al.get(tag).getSingleStringValueOrEmptyString.trim.equals(value)
    }

//    val j0 = is(TagFromName.Manufacturer, "Philips".trim)
//    val j1 = is(TagFromName.Modality, "CT")
//    val j2 = is(TagFromName.InstitutionAddress, "Ann Arbor, MI".trim)

    is(TagFromName.Manufacturer, "Philips".trim) &&
      is(TagFromName.Modality, "CT") &&
      is(TagFromName.InstitutionAddress, "Ann Arbor, MI".trim)
  }

  private def check(f: File): Unit = {
    fileCount = fileCount + 1
    loggy("Checking " + f.getAbsolutePath + "   filecount: " + fileCount)
    try {
      val al = readFile(f)
      if (isPhilips(al)) {
        sum = sum + f.length
        loggy("size: " + f.length + " Date: " + al.get(TagFromName.AcquisitionDate).getSingleStringValueOrEmptyString + " sum: " + sum)
      } else loggy("not Philips sum: " + sum)
    } catch {
      case t: Throwable => loggy("badness: " + t)
    }
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    val mainDir = new File("""\\CONQUESTROSPR1\e$\Dicom\data""")

//    val one = new File("""\\CONQUESTROSPR1\e$\Dicom\data\$CT_SIM\1.3.46.670589.33.1.63709137401335952600002.5239270016059145866_0201_000005_15735580239185.dcm""")
//
//    check(one)

    def doSubDir(subDir: File) = {
      loggy("doing subDir " + subDir.getAbsolutePath)

      try {
        subDir.listFiles.map(f => check(f))
      } catch {
        case t: Throwable => loggy("subdir badness: " + subDir.getAbsolutePath + " : " + t)
      }
    }

    mainDir.listFiles.toSeq.filter(d => d.isDirectory).map(subDir => doSubDir(subDir))

    loggy("Elapsed ms: " + (System.currentTimeMillis - start))
    logOut.close
  }

}