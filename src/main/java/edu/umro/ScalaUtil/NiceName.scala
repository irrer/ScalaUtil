
package edu.umro.ScalaUtil

import java.io.File

/**
 * Example of a program that reads a DICOM file, adds an attribute, and then writes it.
 *
 * @author irrer
 *
 */

object NiceName {

  private val start = System.currentTimeMillis

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
    if (newFile.isDirectory) newFile.listFiles.map(f => niceName(f))
  }

  def main(args: Array[String]): Unit = {
    val fileList = args.map(a => new File(a))
    fileList.map(file => niceName(file))
    println("Elapsed ms: " + (System.currentTimeMillis - start) + "    Total files: " + total + "    Files renamed: " + renamed)
  }

}
