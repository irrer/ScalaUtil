package test.java;

import java.io.File;
import com.pixelmed.dicom.AttributeList;
import com.pixelmed.dicom.TagFromName;
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy
import com.pixelmed.dicom.AttributeTag

/**
 * @author irrer
 *
 */

class DicomReadStrategy extends ReadTerminationStrategy {

  def terminate(attributeList: AttributeList, tag: AttributeTag, bytesRead: Long): Boolean = {
    attributeList.get(TagFromName.SOPInstanceUID) != null
  }
}

object FindDupUIDs {
  val start = System.currentTimeMillis

  val strat = new DicomReadStrategy

  private val inDirName = """\\rodicom1\data"""
  //private val inDirName = """D:\pf\ConQuest\dicomserver1419d1\data"""

  private val inDir = new File(inDirName)

  private case class FileUid(file: File, uid: String);

  private def fileToSopInstanceUid(file: File): Option[FileUid] = {
    try {
      val al = new AttributeList
      al.read(file, strat)
      val uid = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
      if (uid == null) None else Some(new FileUid(file, uid))
    } catch {
      case t: Throwable => None
    }
  }

  private def processDir(dir: File): Unit = {
    val fileList = dir.listFiles
    println("Processing directory: " + dir.getAbsolutePath + "   files: " + fileList.size)
    val fileUidList = fileList.map(file => fileToSopInstanceUid(file)).flatten
    val uidList = fileUidList.map(_.uid)
    def showDup(dup: String) = {
      val bad = fileUidList.filter(fu => fu.uid.equalsIgnoreCase(dup))
      println("    " + bad.mkString("\n    ") + "\n")
    }

    val dupList = uidList.diff(uidList.toSet.toList)

    dupList.map(dup => showDup(dup))
    println("    Number of duplicates: " + dupList.size)
  }

  def main(args: Array[String]): Unit = {

    val dirList = inDir.listFiles.filter(f => f.isDirectory)
    println("number of input dirs: " + dirList.size)

    dirList.map(dir => processDir(dir))

    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
  }

}
