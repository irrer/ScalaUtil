package edu.umro.ScalaUtil

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

object ConstructMhd {

  def revBits(i: Int): Int = {
    val r = ((i >> 7) & 1) |
      ((i >> 5) & 2) |
      ((i >> 3) & 4) |
      ((i >> 1) & 8) |
      ((i << 1) & 16) |
      ((i << 3) & 32) |
      ((i << 5) & 64) |
      ((i << 7) & 128)
    r & 255
  }

  case class Mhd(file: File) {
    val lineList = Utility.readFile(file).split("\n").toList
    def toKv(line: String) = (line.split(" ").head, line.split("=")(1).trim.split(" ").toSeq)
    private val kvMap = lineList.map(line => toKv(line)).toMap
    def get(key: String) = kvMap(key)

    val DimSize = get("DimSize").map(s => s.toInt)
    val ElementSpacing = get("ElementSpacing").map(s => s.toDouble)
    val Offset = get("Offset").map(s => s.toDouble)

    override def toString = {
      kvMap.keys.toSeq.map(k => k + " : " + kvMap(k).mkString(", ")).mkString("\n    ", "\n    ", "")
    }
  }

  def makeSeries(mhd: Mhd, image: Array[Byte], outDir: File, patientId: String) = {
    val transferSyntax = TransferSyntax.ImplicitVRLittleEndian
    val StudyInstanceUID = UMROGUID.getUID
    val SeriesInstanceUID = UMROGUID.getUID
    val FrameOfReferenceUID = UMROGUID.getUID
    val now = new Date
    val date = UMROGUID.dicomDate(now)
    val time = UMROGUID.dicomTime(now)
    val dateTime = date + time + ".000"

    def makeSlice(sliceIndex: Int) = {
      val SOPInstanceUID = UMROGUID.getUID
      val al = new AttributeList
      val file = new File(outDir, (sliceIndex + 1).formatted("CT_%03d.dcm"))

      def makeM(tag: AttributeTag, text: Seq[String]): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        text.map(t => a.addValue(t))
        al.put(a)
      }

      def make(tag: AttributeTag, text: String): Unit = makeM(tag, Seq(text))

      def make0(tag: AttributeTag): Unit = makeM(tag, Seq[String]())

      def makeIntM(tag: AttributeTag, int: Seq[Int]): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        int.map(i => a.addValue(i))
        al.put(a)
      }

      def makeInt(tag: AttributeTag, int: Int): Unit = makeIntM(tag, Seq(int))

      def makeDblM(tag: AttributeTag, dbl: Seq[Double]): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        dbl.map(i => a.addValue(i))
        al.put(a)
      }

      def makeDbl(tag: AttributeTag, dbl: Double): Unit = makeDblM(tag, Seq(dbl))

      def makePixels: Unit = {
        val size = mhd.DimSize(0) * mhd.DimSize(1) * 2
        val pix = image.drop(size * sliceIndex).take(size)

        //        def getPix(i: Int): Short = {
        //          //val p = (((pix(i+1) & 255) << 16) + (pix(i) & 255))
        //          val hi = (pix(i) & 255) << 16
        //          val lo = pix(i + 1) & 255
        //          val hi2 = revBits(lo)
        //          val lo2 = revBits(hi)
        //
        //          val p = (hi2 + lo2) & 0xffff
        //          p.toShort
        //        }
        //        val shrt = (0 until size by 2).map(i => getPix(i)).toArray
        //        val a = new OtherWordAttribute(TagFromName.PixelData)
        //        a.setValues(shrt)

        val a = new OtherByteAttribute(TagFromName.PixelData)
        a.setValues(pix)

        al.put(a)
      }

      val SliceLocation = mhd.Offset(2) + (sliceIndex * mhd.ElementSpacing(2))

      make(TagFromName.MediaStorageSOPClassUID, SOPClass.CTImageStorage)
      make(TagFromName.MediaStorageSOPInstanceUID, SOPInstanceUID)
      make(TagFromName.TransferSyntaxUID, transferSyntax)

      make(TagFromName.SpecificCharacterSet, "ISO_IR 100")
      makeM(TagFromName.ImageType, Seq("ORIGINAL", "PRIMARY", "AXIAL"))
      make(TagFromName.InstanceCreationDate, date)
      make(TagFromName.InstanceCreationTime, time)
      make(TagFromName.SOPClassUID, SOPClass.CTImageStorage)
      make(TagFromName.SOPInstanceUID, SOPInstanceUID)
      make(TagFromName.StudyDate, date)
      make(TagFromName.SeriesDate, date)
      make(TagFromName.AcquisitionDate, date)
      make(TagFromName.ContentDate, date)
      make(TagFromName.AcquisitionDateTime, dateTime)
      make(TagFromName.StudyTime, time)
      make(TagFromName.SeriesTime, time)
      make(TagFromName.AcquisitionTime, time)
      make(TagFromName.ContentTime, time)
      makeInt(TagFromName.AccessionNumber, 1)
      make(TagFromName.Modality, "CT")
      make(TagFromName.Manufacturer, "JimIrrer")
      make(TagFromName.InstitutionName, "Mich Medicine Rad Onc")
      make(TagFromName.InstitutionAddress, "Ann Arbor, MI")
      make(TagFromName.StationName, "none")
      make(TagFromName.StudyDescription, "none")
      make(TagFromName.SeriesDescription, "none")
      make(TagFromName.InstitutionalDepartmentName, "Rad Onc")
      make(TagFromName.OperatorsName, "Rocky")
      make(TagFromName.ManufacturerModelName, "Fabricate from MHD")
      make(TagFromName.PatientName, patientId)
      make(TagFromName.PatientID, patientId)
      make(TagFromName.PatientBirthDate, "18000101")
      make(TagFromName.ReferringPhysicianName, "none")
      make(TagFromName.PatientSex, "O")
      make(TagFromName.SliceThickness, mhd.ElementSpacing(2).toString)
      makeDbl(TagFromName.KVP, 120)
      makeDbl(TagFromName.SpacingBetweenSlices, mhd.ElementSpacing(2).toDouble)
      make(TagFromName.DeviceSerialNumber, "001")
      make(TagFromName.SoftwareVersions, "0.0.1")
      make(TagFromName.PatientPosition, "HFS")
      make(TagFromName.AcquisitionType, "SPIRAL")
      make(TagFromName.StudyInstanceUID, StudyInstanceUID)
      make(TagFromName.SeriesInstanceUID, SeriesInstanceUID)
      make(TagFromName.StudyID, "none")
      makeInt(TagFromName.SeriesNumber, 1)
      makeInt(TagFromName.InstanceNumber, sliceIndex + 1)
      makeDblM(TagFromName.ImagePositionPatient, Seq(mhd.Offset(0), mhd.Offset(1), SliceLocation))
      makeM(TagFromName.ImageOrientationPatient, Seq("1", "0", "0", "0", "1", "0"))
      make(TagFromName.FrameOfReferenceUID, FrameOfReferenceUID)
      if (true) {
        val a = AttributeFactory.newAttribute(TagFromName.PositionReferenceIndicator)
        a.addValue(0.toLong)
        al.put(a)
      } else makeIntM(TagFromName.PositionReferenceIndicator, Seq[Int]())
      makeDbl(TagFromName.SliceLocation, SliceLocation)
      makeInt(TagFromName.SamplesPerPixel, 1)
      make(TagFromName.PhotometricInterpretation, "MONOCHROME2")
      makeInt(TagFromName.Rows, mhd.DimSize(1))
      makeInt(TagFromName.Columns, mhd.DimSize(0))
      makeM(TagFromName.PixelSpacing, mhd.ElementSpacing.take(2).map(d => d.toString))
      makeInt(TagFromName.BitsAllocated, 16)
      makeInt(TagFromName.BitsStored, 16)
      makeInt(TagFromName.HighBit, 15)
      makeInt(TagFromName.PixelRepresentation, 1)
      makeInt(TagFromName.AcquisitionNumber, 1)
      //    makeDblM(TagFromName.WindowCenter, (Seq(60, 60)))
      //    makeDblM(TagFromName.WindowWidth, (Seq(400, 400)))
      makeDbl(TagFromName.RescaleIntercept, 0)
      makeDbl(TagFromName.RescaleSlope, 1)
      makePixels

      //DicomUtil.writeAttributeList(al, file)

      //al.write(new FileOutputStream(file), transferSyntax, true, true)
      FileMetaInformation.addFileMetaInformation(al, transferSyntax, "Mhd");

      DicomUtil.writeAttributeList(al, file)
      println("Created " + file.getAbsolutePath)
    }

    for (sliceIndex <- (0 until mhd.DimSize(2))) {
      makeSlice(sliceIndex)
    }
  }

  private def usage(msg: String) = {
    println(msg)
    System.exit(1)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    if (args.size != 3) usage("Must give three files, an MHD file and an image file and a patient ID.")
    val mhdFileName = args(0)
    val imageFileName = args(1)
    val patientId = args(2)

    val mhdFile = new File(mhdFileName)
    val imageFile = new File(imageFileName)

    if (!mhdFile.canRead) usage("Can not read MHD file: " + mhdFile.getAbsolutePath)
    if (!imageFile.canRead) usage("Can not read image file: " + imageFile.getAbsolutePath)

    // output directory named after MHD file
    val outDir = {
      val parent = mhdFile.getParent
      val name = mhdFile.getName.dropRight(4) + "Dicom"
      val od = new File(parent, name)
      od
    }
    Utility.deleteFileTree(outDir)
    outDir.mkdirs
    println("Using MHD file: " + mhdFile.getAbsolutePath + "    image file: " + imageFile.getAbsolutePath + "    output: " + outDir.getAbsolutePath)
    val mhd = new Mhd(mhdFile)
    println("mhd: " + mhd)
    val imageBytes = Utility.readBinFile(imageFile)
    makeSeries(mhd, imageBytes, outDir, patientId)

    // (0 until 256).map(i => println("i: " + i.formatted("%4d") + " : " + i.formatted("%4x") + " :: " + revBits(i).formatted("%4d") + " : " + revBits(i).formatted("%4x")))

    println("Elapsed ms: " + (System.currentTimeMillis - start))
  }

}