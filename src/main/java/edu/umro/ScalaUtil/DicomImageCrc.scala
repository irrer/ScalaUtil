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
import com.pixelmed.dicom.OtherByteAttribute
import com.pixelmed.dicom.OtherLongAttribute
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.DicomDict.TagByName

import java.io.File

/**
  * Create a hash of the image pixels for all given DICOM files.
  *
  * A simple 64 bit CRC algorithm is used, where the accumulating value is
  * bitwise shifted left one bit, and then XOR with the new byte.
  *
  * The caret operator in Scala (upper case) is XOR.
  */
object DicomImageCrc {

  private var count = 0

  /**
    * If the given files is DICOM image file, then print the file name and a CRC of its image bytes.
    *
    * @param file DICOM file being input.
    * @param al   Attribute list of DICOM file.
    */
  private def alToCrc(file: File, al: AttributeList): Unit = {
    val pixelData = al.get(TagByName.PixelData)
    if (pixelData != null) {

      var crc: Long = 0

      if (pixelData.isInstanceOf[OtherByteAttribute]) {
        val byteArray = pixelData.getByteValues
        byteArray.foreach(b => { crc = (crc << 1) ^ (b.toInt & 0xff) })
      }

      if (pixelData.isInstanceOf[OtherWordAttribute]) {
        val shortArray = pixelData.getShortValues
        shortArray.foreach(b => { crc = (crc << 1) ^ (b.toInt & 0xffff) })
      }

      if (pixelData.isInstanceOf[OtherLongAttribute]) {
        val intArray = pixelData.getIntegerValues
        intArray.foreach(b => { crc = (crc << 1) ^ (b & 0xffffffff) })
      }

      val crcText = "%016x".format(crc)
      val filePath = file.getAbsolutePath
      println(s"$crcText : $filePath")

      count = count + 1
    }
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
      alToCrc(dicomFile, al)
    } catch {
      case _: Throwable =>
        System.err.println("Unable to read file as DICOM.  Ignoring: " + dicomFile.getAbsolutePath)
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
      println("\nDone.  Elapsed ms: " + (System.currentTimeMillis - start) + "    number of DICOM image files: " + count)
    } catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }

}
