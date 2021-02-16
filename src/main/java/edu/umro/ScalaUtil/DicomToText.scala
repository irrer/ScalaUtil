package edu.umro.ScalaUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.DicomFileUtilities

import java.io.File


/**
 * Create text files from
 */
object DicomToText {

  private val textFileSuffix = ".txt"

  private var count = 0

  private def makeName(file: File): String = {

    val dicomFileName = file.getName
    val suffixStart = dicomFileName.lastIndexOf('.')

    def suffixIsNumeric(): Boolean = dicomFileName.drop(suffixStart + 1).replaceAll("[0-9]", "").isEmpty

    if ((suffixStart != -1) && !suffixIsNumeric())
      dicomFileName.take(suffixStart) + textFileSuffix
    else
      dicomFileName + textFileSuffix
  }


  /**
   * Convert an attribute list to text and write it to a file.
   *
   * @param file DICOM file being input.
   * @param al   Attribute list of DICOM file.
   */
  private def alToTextFile(file: File, al: AttributeList): Unit = {
    val textFileName = makeName(file)
    val textFile = new File(file.getParentFile, textFileName)
    if (!textFile.exists) {
      try {
        FileUtil.writeFile(textFile, DicomUtil.attributeListToString(al))
        println("Wrote file: " + textFile.getAbsolutePath)
        count = count + 1
      }
      catch {
        case t: Throwable =>
          println("Error writing file " + textFile.getAbsolutePath + " : " + t)
      }
    }
  }


  /**
   * If a file is DICOM, then make text file for it.
   *
   * @param dicomFile DICOM file being input.
   */
  private def readFile(dicomFile: File): Unit = {
    try {
      if (DicomFileUtilities.isDicomOrAcrNemaFile(dicomFile)) {
        val al = new AttributeList
        al.read(dicomFile)
        alToTextFile(dicomFile, al)
      }
    } catch {
      case _: Throwable =>
        println("Unable to read file as DICOM.  Ignoring: " + dicomFile.getAbsolutePath)
    }
  }


  /**
   * Recursively search a file tree for DICOM files.  For each, write a text file in the same directory.
   *
   * @param file Top level file.  May be a file or directory.
   */
  private def processFilesInTree(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles.foreach(f => processFilesInTree(f))
    else
      readFile(file)
  }


  def main(args: Array[String]): Unit = {
    try {
      val start = System.currentTimeMillis
      if (args.isEmpty)
        println("Create text versions of DICOM files.\nUsage: d2text myFolder myFile.dcm ... ")
      args.foreach(a => processFilesInTree(new File(a)))
      processFilesInTree(new File("""D:\tmp\aqa\GapSkew\d2"""))
      println("\nDone.  Elapsed ms: " + (System.currentTimeMillis - start) + "    number of files written: " + count)
    } catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }

}
