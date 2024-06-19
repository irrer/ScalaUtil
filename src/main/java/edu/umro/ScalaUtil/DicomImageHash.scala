/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.umro.ScalaUtil

import com.pixelmed.dicom.AttributeList

import java.io.File

/**
  * Create an MD5 hash of the image pixels for all given DICOM files and, recursively, the directories.
  */
object DicomImageHash {

  /**
    * If the given files is DICOM image file, then print the file name and a hash of its image bytes.
    *
    * @param file DICOM file being input.
    * @param al   Attribute list of DICOM file.
    */
  private def alToHash(file: File, al: AttributeList): Unit = {
    val byteArray = DicomUtil.PixelDataToByteArray(al)
    val hash = Crypto.hash(byteArray)

    val hashText = RawByte.formatByteArray(hash)
    val filePath = file.getAbsolutePath
    println(s"$hashText : $filePath")
  }

  /**
    * If a file is DICOM, then make text file for it.
    *
    * @param dicomFile DICOM file being input.
    */
  private def readFile(dicomFile: File): Unit = {
    try {
      val al = new AttributeList
      al.read(dicomFile)
      alToHash(dicomFile, al)
    } catch {
      case _: Throwable =>
      // System.err.println("Unable to read file as DICOM.  Ignoring: " + dicomFile.getAbsolutePath)
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
      if (args.isEmpty)
        println("Create text versions of DICOM files.\nUsage: d2text myFolder myFile.dcm ... ")
      args.foreach(a => processFilesInTree(new File(a)))
      processFilesInTree(new File("""D:\tmp\aqa\GapSkew\d2"""))
      // println("\nDone.  Elapsed ms: " + (System.currentTimeMillis - start) + "    number of DICOM image files: " + count)
    } catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }

}
