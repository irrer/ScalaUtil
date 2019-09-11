
package edu.umro.ScalaUtil

import java.io.File
import com.pixelmed.dicom.DicomFileUtilities
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.AttributeTag

/**
 * Given a struct, plan, and optionally dose file, create new versions of them that connect them as a set.
 *
 * This is definitely not playing by the DICOM immutability rules, so the oneness is placed on the user to do the right thing.
 *
 * The files are rejected if they do not share the same frame of reference.
 *
 * @author irrer
 *
 */

object DicomLinker extends Logging {

  val start = System.currentTimeMillis

  val outputDirName = "output"

  private val regEx = "[^a-zA-Z0-9_\\.\\-]"
  private val goodChar = '_'.toString

  private var total = 0

  private var renamed = 0

  private def niceName(file: File): Unit = {

    total = total + 1
    //Trace.trace("File: " + file.getAbsolutePath) // TODO rm
    val oldName = file.getName
    val newName = oldName.replaceAll(regEx, goodChar)
    val newFile: File = if (!oldName.equals(newName)) {
      val nf = new File(file.getParentFile, newName)
      if (file.renameTo(nf)) println("renamed " + file.getAbsolutePath + " -> " + nf.getAbsolutePath)
      renamed = renamed + 1
      nf
    } else file
    //Trace.trace("newFile isDir: " + newFile.isDirectory + " : " + newFile.getAbsolutePath) // TODO rm
    if (newFile.isDirectory) newFile.listFiles.map(f => niceName(f))
  }

  private def fail(msg: String) {
    println(msg)
    System.exit(1)
  }

  private def getFileList(args: Array[String]): Seq[File] = {
    val emptyList = Seq[File]()

    def readFiles(file: File): Seq[File] = {
      try {
        if (DicomFileUtilities.isDicomOrAcrNemaFile(file))
          Seq(file)
        else {
          if (file.isDirectory) {
            FileUtil.listFiles(file).map(f => readFiles(f)).flatten
          } else
            emptyList
        }
      } catch {
        case t: Throwable => {
          println("Unexpected exception: " + t)
          println(fmtEx(t))
          emptyList
        }
      }
    }
    args.toSeq.map(a => readFiles(new File(a))).flatten
  }

  private def makeOutputDir(args: Array[String]): File = {
    val list = args.map(f => new File(f)).filter(f => f.exists)
    if (list.isEmpty) fail("No files given.  Must give list of DICOM files or folders.")

    val topDir = list.head.getParentFile
    val outputDir = new File(topDir, outputDirName)
    outputDir.mkdirs
    if (!outputDir.isDirectory) {
      println("Unable to make output dir in " + topDir.getAbsolutePath)
      System.exit(1)
    }
    outputDir
  }

  private case class DicomFile(file: File) {
    val attributeList = new AttributeList
    attributeList.read(file)

    val Modality = attributeList.get(TagFromName.Modality).getSingleStringValueOrEmptyString
    val MediaStorageSOPClassUID = attributeList.get(TagFromName.MediaStorageSOPClassUID).getSingleStringValueOrEmptyString
    val SOPInstanceUID = attributeList.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString.trim

    val isStruct = MediaStorageSOPClassUID.equals(SOPClass.RTStructureSetStorage)
    val isPlan = MediaStorageSOPClassUID.equals(SOPClass.RTPlanStorage)
    val isDose = MediaStorageSOPClassUID.equals(SOPClass.RTDoseStorage)

    def FrameOfReferenceUID = {
      val foruid = attributeList.get(TagFromName.FrameOfReferenceUID)
      if (foruid != null) foruid.getSingleStringValueOrEmptyString.trim
      else {
        val seq = DicomUtil.seqToAttr(attributeList, TagFromName.ReferencedFrameOfReferenceSequence)
        if (seq == null) {
          fail("No frame of reference for " + file.getAbsolutePath)
          ""
        } else {
          seq.map(al => al.get(TagFromName.FrameOfReferenceUID)).filter(at => at != null).map(at => at.getSingleStringValueOrEmptyString).head
        }
      }
    }

    override def toString = {
      "file: " + file.getName +
        "    Modality: " + Modality +
        "    isStruct: " + isStruct +
        "    isPlan: " + isPlan +
        "    isDose: " + isDose +
        "    FrameOfReferenceUID: " + FrameOfReferenceUID +
        "    SOPInstanceUID: " + SOPInstanceUID
    }

    def write(outputDir: File): Unit = {
      val outFile = new File(outputDir, file.getName)
      outFile.delete
      println("Writing DICOM file " + outFile.getAbsolutePath)
      DicomUtil.writeAttributeListToFile(attributeList, outFile, "MROQC Linker")
    }
  }

  private def referTo(dicomFile: DicomFile, sequenceTag: AttributeTag, uid: String): Unit = {
    val seq = DicomUtil.seqToAttr(dicomFile.attributeList, sequenceTag).head
    val at = seq.get(TagFromName.ReferencedSOPInstanceUID)
    at.removeValues
    at.addValue(uid)
  }

  private def refStruct(dicomFile: DicomFile, struct: DicomFile): Unit = {
    referTo(dicomFile, TagFromName.ReferencedStructureSetSequence, struct.SOPInstanceUID)
  }

  private def refPlan(dicomFile: DicomFile, plan: DicomFile): Unit = {
    referTo(dicomFile, TagFromName.ReferencedRTPlanSequence, plan.SOPInstanceUID)
  }

  private def linkDicomFiles(dicomList: Seq[DicomFile], outputDir: File): Unit = {
    val structList = dicomList.filter(df => df.isStruct)
    val planList = dicomList.filter(df => df.isPlan)
    val doseList = dicomList.filter(df => df.isDose)

    if (structList.size != 1) fail("Exactly one struct files must be given, but found " + structList.size)
    if (planList.size != 1) fail("Exactly one plan files must be given, but found " + planList.size)
    if (doseList.size > 1) fail("Only 0 or 1 dose files must be given, but found " + planList.size)

    val allDicomFiles = Seq(structList, planList, doseList).flatten

    println("processing " + allDicomFiles.size + " files:\n    " + allDicomFiles.mkString("\n    "))

    val frameOfRefList = allDicomFiles.map(df => df.FrameOfReferenceUID).distinct
    if (frameOfRefList.size > 1) fail("Files are not all using the same frame of reference")

    refStruct(planList.head, structList.head)

    if (doseList.size == 1) {
      refStruct(doseList.head, structList.head)
      refPlan(doseList.head, planList.head)
    }

    allDicomFiles.map(df => df.write(outputDir))
  }

  def main(args: Array[String]): Unit = {
    try {
      if (args.isEmpty) {
        println("No files given.  Must give list of DICOM files or folders.")
        System.exit(1)
      }

      val outputDir = makeOutputDir(args)
      val fileList = getFileList(args)

      if (fileList.isEmpty) {
        println("No DICOM files found.")
        System.exit(1)
      }

      println("Processing DICOM files\n    " + fileList.map(f => f.getAbsolutePath).mkString("\n    "))
      linkDicomFiles(fileList.map(f => new DicomFile(f)), outputDir)

    } catch {
      case t: Throwable => {
        println("Unexpected exception: " + t)
        println(fmtEx(t))
        fail("processing aborted")
      }
    }
    println("Elapsed ms: " + (System.currentTimeMillis - start) + "    Total files: " + total + "    Files renamed: " + renamed)
    System.exit(0)
  }

}
