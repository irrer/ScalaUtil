package edu.umro.ScalaUtil.DicomTree

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.FileUtil

import java.io.File

object DicomTree {

  /** When creating a single DICOM file, use this as a suffix. */
  val dicomFileSuffix = ".dcm"


  /** Maximum number of characters to use for descriptions.  They can be hundreds of characters, which
   * makes unreasonably long file names.
   */

  /** Maximum number of characters for study descriptions. */
  val maxStudyDescriptionSize = 50


  /** Maximum number of characters for series descriptions. */
  val maxSeriesDescriptionSize = 30

  /**
   * Read a DICOM file.  If it contains DICOM, then return the corresponding attribute list.
   *
   * @param dicomFile Try to read this as a DICOM file.
   * @return An attribute list or  nothing on failure.
   */
  private def readFile(dicomFile: File): Option[AttributeList] = {
    try {
      val al = new AttributeList
      al.read(dicomFile)
      print(".") // show read progress to user
      Some(al)
    } catch {
      case _: Throwable =>
        println("Unable to read file as DICOM.  Ignoring: " + dicomFile.getAbsolutePath)
        None
    }
  }

  /**
   * Add the references of the given file to the data structures.
   *
   * @param file Read this file.
   */
  private def addFile(file: File): Unit = {
    readFile(file) match {
      case Some(al) =>
        print(".")
        val PatientID = TreeUtil.getAttr(al, TagFromName.PatientID)
        if (!PatientMap.contains(PatientID)) PatientMap.put(PatientID, Patient(PatientID))
        PatientMap.get(PatientID).add(file, al)
      case _ =>
    }
  }


  /**
   * Crawl the file tree, adding any DICOM files to the data structures.
   *
   * @param file File in input tree.
   */
  private def findFilesInTree(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles.foreach(f => findFilesInTree(f))
    else
      addFile(file)
  }


  /**
   * Main entry point of application.  Check args and start processing.
   *
   * @param args Input dir and, optionally, output dir.
   */
  def main(args: Array[String]): Unit = {
    try {
      val start = System.currentTimeMillis
      if ((args.length < 1) || (args.length > 2)) {
        println("Usage: treeify inputdir outputdir\nNo files moved.")
        System.exit(1)
      }
      val inDir = new File(args(0))

      // If the output dir was specified, then use it.  Otherwise use the input dir appended with "output".
      val outDir = {
        if (args.length == 2) {
          new File(args(1))
        }
        else {
          val outName = inDir.getName + "output"
          new File(inDir.getParentFile, outName)
        }
      }

      findFilesInTree(inDir)

      if (PatientMap.size < 2)
        PatientMap.values.foreach(patient => patient.move(outDir))
      else
        PatientMap.values.foreach(patient => patient.move(new File(outDir, FileUtil.replaceInvalidFileNameCharacters(patient.PatientID, replacement = '_'))))

      println("\nDone.  Elapsed ms: " + (System.currentTimeMillis - start))
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }

}
