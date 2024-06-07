/*
 * Copyright 2024 Regents of the University of Michigan
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

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.OtherWordAttribute
import com.pixelmed.dicom.SOPClass
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.util.UMROGUID

import java.io.File
import java.util.Date
import scala.collection.mutable.ArrayBuffer

object AngioFix {

  private val DistanceBetweenImages = 1.5

  private def makeAttr(tag: AttributeTag, value: String): Attribute = {
    val attr = AttributeFactory.newAttribute(tag)
    attr.addValue(value)
    attr
  }

  private def makeUidAttr(tag: AttributeTag): Attribute = makeAttr(tag, UMROGUID.getUID)

  private def sortByContentTime(dcmFile: File): Long = {
    val attributeList = new AttributeList
    attributeList.read(dcmFile)
    DicomUtil.getTimeAndDate(attributeList, TagByName.ContentDate, TagByName.ContentTime).get.getTime
  }

  private def fixDicom(outDir: File, dcmFile: File, replacementList: Seq[Attribute], index: Int, sliceCount: Int): Unit = {
    val attributeList = new AttributeList
    attributeList.read(dcmFile)
    replacementList.map(at => attributeList.put(at)) // actually replace attributes
    val sop = makeUidAttr(TagByName.SOPInstanceUID)
    attributeList.put(makeUidAttr(TagByName.SOPInstanceUID))
    attributeList.put(makeAttr(TagByName.MediaStorageSOPInstanceUID, sop.getSingleStringValueOrEmptyString))
    attributeList.remove(TagByName.IssuerOfPatientID)

    val pixelList = attributeList.get(TagByName.PixelData).getByteValues()

    val len = pixelList.length / 3

    val bwPixelList = new ArrayBuffer[Short]

    val mask8Bit = 0xff

    (0 until len).foreach(i => {
      val i3 = i * 3
      val p0 = pixelList(i3) & mask8Bit
      val p1 = pixelList(i3 + 1) & mask8Bit
      val p2 = pixelList(i3 + 2) & mask8Bit

      // set all colored voxels to 0
      val pix = if ((p0 != p1) || (p1 != p2)) 0 else p0 + p1 + p2

      bwPixelList.append(pix.asInstanceOf[Short])
    })

    val owPixelData = new OtherWordAttribute(TagByName.PixelData)

    attributeList.remove(TagByName.PixelData)

    val ImagePositionPatient = {
      val Rows = attributeList.get(TagByName.Rows).getIntegerValues.head
      val Columns = attributeList.get(TagByName.Columns).getIntegerValues.head
      val PixelSpacing = attributeList.get(TagByName.PixelSpacing).getDoubleValues
      val xPixelSpacing = PixelSpacing.head
      val yPixelSpacing = PixelSpacing(1)

      val x = -(((Rows / 2) - 0.5) * xPixelSpacing)
      val y = -(((Columns / 2) - 0.5) * yPixelSpacing)

      val z = -(((sliceCount / 2) - 0.5) * DistanceBetweenImages) + (index * DistanceBetweenImages)
      val attr = AttributeFactory.newAttribute(TagByName.ImagePositionPatient)
      attr.addValue(x)
      attr.addValue(y)
      attr.addValue(z)

      attr
    }

    val SeriesDescription = attributeList.get(TagByName.SeriesDescription)
    val desc = SeriesDescription.getSingleStringValueOrEmptyString() + " as CT"
    SeriesDescription.removeValues()
    SeriesDescription.addValue(desc)

    attributeList.put(ImagePositionPatient)

    owPixelData.setValues(bwPixelList.toArray)
    attributeList.put(owPixelData)

    val newFile = new File(outDir, dcmFile.getName)
    DicomUtil.writeAttributeListToFile(attributeList, newFile, "AngioFix")
    println("    Created file " + newFile.getName)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    val inDir = new File("""D:\tmp\angioImport\000624638""")
    println("Using input dir: " + inDir.getAbsolutePath)

    val outDir = new File("""D:\tmp\angioImport\101942236Out""")
    println("Output dir: " + outDir.getAbsolutePath)
    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs
    Thread.sleep(250)

    // 0020,0037 DS  ImageOrientationPatient: 1 \ 0 \ 0 \ 0 \ 1 \ 0
    val ImageOrientationPatient = {
      val attr = AttributeFactory.newAttribute(TagByName.ImageOrientationPatient)
      attr.addValue(1)
      attr.addValue(0)
      attr.addValue(0)
      attr.addValue(0)
      attr.addValue(1)
      attr.addValue(0)
      attr
    }

    val ImageType = {
      val attr = AttributeFactory.newAttribute(TagByName.ImageType)
      attr.addValue("ORIGINAL")
      attr.addValue("PRIMARY")
      attr.addValue("AXIAL")
      attr
    }

    val now = new Date

    val SeriesDate = {
      val attr = AttributeFactory.newAttribute(TagByName.SeriesDate)
      attr.addValue(DicomUtil.dicomDateFormat.format(now))
      attr
    }

    val SeriesTime = {
      val attr = AttributeFactory.newAttribute(TagByName.SeriesTime)
      attr.addValue(DicomUtil.dicomTimeFormat.format(now))
      attr
    }

    val RescaleIntercept = {
      val attr = AttributeFactory.newAttribute(TagByName.RescaleIntercept)
      attr.addValue(0)
      attr
    }

    val RescaleSlope = {
      val attr = AttributeFactory.newAttribute(TagByName.RescaleSlope)
      attr.addValue(1)
      attr
    }

    val RescaleType = {
      val attr = AttributeFactory.newAttribute(TagByName.RescaleType)
      attr.addValue("HU")
      attr
    }

    val PositionReferenceIndicator = {
      val attr = AttributeFactory.newAttribute(TagByName.PositionReferenceIndicator)
      attr
    }

    val replacementList = Seq(
      makeAttr(TagByName.PatientID, "101942236"),
      // makeAttr(TagByName.PatientID, "$AngioFix"), // TODO rm
      // makeAttr(TagByName.PatientName, "$AngioFix"), // TODO rm
      makeAttr(TagByName.PhotometricInterpretation, "MONOCHROME2"),
      makeAttr(TagByName.BitsAllocated, "16"),
      makeAttr(TagByName.BitsStored, "16"),
      makeAttr(TagByName.HighBit, "15"),
      makeAttr(TagByName.PixelRepresentation, "0"),
      makeAttr(TagByName.SamplesPerPixel, "1"),
      // makeAttr(TagByName.MediaStorageSOPClassUID, "1.2.840.10008.5.1.4.1.1.2"),
      makeAttr(TagByName.MediaStorageSOPClassUID, SOPClass.CTImageStorage),
      makeAttr(TagByName.SOPClassUID, SOPClass.CTImageStorage),
      makeAttr(TagByName.Modality, "CT"),
      makeAttr(TagByName.SliceThickness, DistanceBetweenImages.toString),
      makeAttr(TagByName.ExposureTime, "0"),
      makeAttr(TagByName.XRayTubeCurrent, "0"),
      makeAttr(TagByName.KVP, "0"),
      makeAttr(TagByName.ScanOptions, "STANDARD"),
      makeAttr(TagByName.DistanceSourceToDetector, "1500"),
      makeAttr(TagByName.DistanceSourceToPatient, "1000"),
      makeAttr(TagByName.RotationDirection, "CC"),
      ImageOrientationPatient,
      SeriesDate,
      SeriesTime,
      ImageType,
      PositionReferenceIndicator,
      RescaleIntercept,
      RescaleSlope,
      RescaleType,
      makeUidAttr(TagByName.FrameOfReferenceUID),
      makeUidAttr(TagByName.StudyInstanceUID),
      makeUidAttr(TagByName.SeriesInstanceUID),
      makeUidAttr(TagByName.FrameOfReferenceUID)
    )

    val fileList = FileUtil.listFiles(inDir).sortBy(sortByContentTime)

    fileList.zipWithIndex.foreach(inFileIndex => {
      val inFile = inFileIndex._1
      val index = inFileIndex._2
      fixDicom(outDir, inFile, replacementList, index, fileList.size)
    })
    println("Output dir: " + outDir.getAbsolutePath)

    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
    System.exit(0)
  }

}
