package edu.umro.ScalaUtil.DicomTree

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
   * Add the references of the given file to the data structures.
   *
   * @param file Read this file.
   */
  private def addFile(file: File): Unit = {
    try {
      TreeUtil.readFile(file) match {
        case Some(al) =>
          print(".")
          val PatientID = TreeUtil.getAttr(al, TagFromName.PatientID)
          if (!PatientMap.contains(PatientID)) PatientMap.put(PatientID, Patient(PatientID))
          PatientMap.get(PatientID).add(file, al)
        case _ =>
      }
    }
    catch {
      case t: Throwable => println("Can not read file " + file.getAbsolutePath + " as DICOM.  File ignored.")
    }
  }


  /**
   * Crawl the file tree, adding any DICOM files to the data structures.
   *
   * @param file File in input tree.
   */
  private def addFilesInTree(file: File): Unit = {
    if (file.isDirectory)
      TreeUtil.listFilesSafely(file).foreach(f => addFilesInTree(f))
    else
      addFile(file)
  }


  /**
   * Crawl the file tree, deleting any empty directories.
   *
   * @param file File in input tree.
   */
  private def deleteFilesInTree(file: File): Unit = {
    if (file.isDirectory) {
      TreeUtil.listFilesSafely(file).foreach(f => deleteFilesInTree(f))
      if (TreeUtil.listFilesSafely(file).isEmpty)
        file.delete
    }
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
        println("Usage: DicomTree inputDir outputDir\nNo files moved.")
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

      addFilesInTree(inDir)

      if (PatientMap.size < 2)
        PatientMap.values.foreach(patient => patient.move(outDir))
      else
        PatientMap.values.foreach(patient => patient.move(new File(outDir, FileUtil.replaceInvalidFileNameCharacters(patient.PatientID, replacement = '_'))))

      println("\nDeleting empty directories...")
      deleteFilesInTree(inDir)

      println("\nDone.  Elapsed ms: " + (System.currentTimeMillis - start))
    }
    catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }

}
