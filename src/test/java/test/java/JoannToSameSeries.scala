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

import scala.xml.Attribute
import com.pixelmed.dicom.OtherWordAttribute

import java.io.FileOutputStream
import com.pixelmed.dicom.FileMetaInformation
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.OtherByteAttributeOnDisk
import edu.umro.DicomDict.TagByName

object JoannToSameSeries {

  val transferSyntax = TransferSyntax.ImplicitVRLittleEndian

  private def readFile(file: File) = {
    val al = new AttributeList
    println("reading file: " + file.getAbsolutePath)
    al.read(file)
    al
  }

  private def writeFile(al: AttributeList, file: File) = {

    FileMetaInformation.addFileMetaInformation(al, transferSyntax, "irrer")

    DicomUtil.writeAttributeListToFile(al, file, "irrer")
    println("Created " + file.getAbsolutePath)
  }

  private def fix(file: File, index: Int, outDir: File) = {
    val al = readFile(file)
    val fileName = "Fixed_" + file.getName.replace("dcm$", "").replace("DCM$", "") + ".dcm"

    val ImageType = AttributeFactory.newAttribute(TagFromName.ImageType)
    ImageType.addValue("DERIVED")
    ImageType.addValue("SECONDARY")
    ImageType.addValue("SINGLE PLANE")
    ImageType.addValue("SINGLE A")
    ImageType.addValue("REFIMAGE")
    al.put(ImageType)

    al.remove(TagByName.StartTrim)
    al.remove(TagByName.StopTrim)
    al.remove(TagByName.RecommendedDisplayFrameRate)
    al.remove(TagByName.CineRate)
    al.remove(TagFromName.FrameTime)
    al.remove(TagByName.PositionerMotion)
    al.remove(new AttributeTag(0x0021, 0x1028))
    al.remove(new AttributeTag(0x0025, 0x1010))
    al.remove(TagFromName.NumberOfFrames)
    al.remove(TagFromName.FrameIncrementPointer)
    al.remove(TagFromName.ReferencedImageSequence)

    val Rows = al.get(TagFromName.Rows).getIntegerValues.head
    val Columns = al.get(TagFromName.Columns).getIntegerValues.head
    val BitsAllocated = al.get(TagFromName.BitsAllocated).getIntegerValues.head
    val pixSize = Rows * Columns * ((BitsAllocated + 7) / 8)
    val PixelData = (al.get(TagFromName.PixelData)) //.asInstanceOf[OtherWordAttribute]
    PixelData match {
      case wd: OtherWordAttribute => {
        PixelData.setValues(PixelData.getShortValues.take(pixSize / 2))
      }
      case by: OtherByteAttribute => {
        PixelData.setValues(PixelData.getByteValues.take(pixSize))
      }
      case byd: OtherByteAttributeOnDisk => {
        val bytes = PixelData.getByteValues.take(pixSize)
        al.remove(TagFromName.PixelData)
        val pd = new OtherByteAttribute(TagFromName.PixelData)
        pd.setValues(bytes)
        al.put(pd)
      }
      case _ => {
        println("Unexpected type of data in file " + file.getAbsolutePath + " : " + PixelData.getClass)
      }
    }
    if (al.get(TagFromName.PatientPosition) == null)
      println("Not writing file " + file.getAbsolutePath + " because it has no patient position.")
    writeFile(al, new File(outDir, fileName))
  }

  private def processDir(dir: File) = {
    println("processing dir: " + dir.getAbsolutePath)

    val outDir = new File(dir, "out")
    Utility.deleteFileTree(outDir)
    outDir.mkdirs
    Thread.sleep(500)
    println("Made dir: " + outDir.getAbsolutePath + "     isDir: " + outDir.isDirectory)
    val fileList = dir.listFiles.toSeq.filter(f => f.isFile)
    fileList.zipWithIndex.map(ali => fix(ali._1, ali._2 + 1, outDir))
  }

  val dirList = Seq(
    """D:\tmp\joann\COOPER_20190923_134833\DICOM""",
    """D:\tmp\joann\ESTEP_20190923_153243\DICOM""").map(name => new File(name))

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    dirList.map(dir => processDir(dir))

    println("Elapsed ms: " + (System.currentTimeMillis - start))
  }

}