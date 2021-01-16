package test.java

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute

import java.io.File
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.TransferSyntax

import java.io.FileOutputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.DataInputStream
import java.io.BufferedInputStream
import edu.umro.ScalaUtil.Permute
import edu.umro.ScalaUtil.RawByte
import com.pixelmed.dicom.DicomDictionary
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.OtherWordAttribute
import edu.umro.DicomDict.TagByName

// Special for Madhava and Yue: Thu, Dec 08, 2016  3:39:44 PM

object MadhavaEdit {

    private val DEFAULT_TRANSFER_SYNTAX = TransferSyntax.ImplicitVRLittleEndian;

    var wide = 256
    var high = 256
    var patient = "31" // 49

    def wh = wide * high

    private def inDir = new File("""D:\tmp\madhava\""" + patient)
    private def inFile = new File(inDir, "nCBV_map.img")

    private def readFile: Seq[Byte] = {
        val len = inFile.length.toInt
        val numFloat = len / 4
        val fis = new FileInputStream(inFile)
        val bytes = Array.ofDim[Byte](len)
        fis.read(bytes)
        bytes.toSeq
    }

    private def scaleToRange(floatList: Seq[Float], low: Double, high: Double): Seq[Float] = {

        val min = floatList.min
        val max = floatList.max

        val m = (high - low) / (max - min)
        val b = min - (m * min)

        println("min: " + min + "    max: " + max)
        println("m:   " + m + "    b:   " + b)

        def floatToInt(f: Float): Float = {
            val fv = ((f * m) + b).round.toInt match {
                case v if v < low => low
                case v if v > high => high
                case v => v
            }
            fv.toFloat
        }

        floatList.map(f => floatToInt(f))
    }

    val filePrefix = "Out_"

    def saveImage(img: Seq[Int], outFile: File): Unit = {

        val png = new BufferedImage(wide, high, BufferedImage.TYPE_INT_RGB);

        def setPixel(i: Int): Unit = {
            val x = (i % wide)
            val y = i / wide
            png.setRGB(x, y, img(i))
        }
        (0 until wh).map(i => setPixel(i))

        outFile.delete
        outFile.getParentFile.mkdirs
        ImageIO.write(png, "png", outFile)
    }

    def fileOf(i: Int, suffix: String): File = {
        val fileName = filePrefix + i.formatted("%03d.") + suffix
        val file = new File(inDir, fileName)
        file.delete
        file
    }

    def getImg(img: Int, list: Seq[Float]) = list.drop(img * wh).take(wh).map(f => f.round.toInt)

    def printSeq[A](seq: Seq[A]): Unit = {
        (0 until seq.size).map(i => { print("    " + seq(i)); if ((i % 10) == 9) println })
        println
    }

    def getDicomTemplate: AttributeList = {
        val al = new AttributeList
        al.read(new File(inDir, "000000.dcm"))
        al
    }

    val dictionary = new DicomDictionary

    val SeriesInstanceUID = UMROGUID.getUID // makeUID

    def makeDicom31(i: Int, voxList: Seq[Int], numImages: Int) = {
        val ImagePositionPatientX = -114.009
        val ImagePositionPatientY = -142.919
        val minImagePositionPatientZ = -25.8168
        val maxImagePositionPatientZ = 52.1832
        val TemporalPositionIdentifier = i + 1
        val NumberOfTemporalPositions = numImages
        val minTriggerTime = 92
        val maxTriggerTime = 142892

        val al = getDicomTemplate

        al.get(TagFromName.SOPInstanceUID).removeValues
        al.get(TagFromName.SOPInstanceUID).addValue(UMROGUID.getUID)

        val zLoc = minImagePositionPatientZ + (6.5 * i)

        val ipp = al.get(TagFromName.ImagePositionPatient)
        ipp.removeValues
        ipp.addValue(ImagePositionPatientX)
        ipp.addValue(ImagePositionPatientY)
        ipp.addValue(zLoc)

        al.get(TagFromName.TriggerTime).removeValues
        al.get(TagFromName.TriggerTime).addValue(minTriggerTime + (1200 * i))

        al.get(TagFromName.InstanceNumber).removeValues
        al.get(TagFromName.InstanceNumber).addValue(i + 1)

        al.get(TagByName.TemporalPositionIdentifier).removeValues
        al.get(TagByName.TemporalPositionIdentifier).addValue(i)

        al.get(TagFromName.SliceLocation).removeValues
        al.get(TagFromName.SliceLocation).addValue(zLoc)

        val PixelData = al.get(TagFromName.PixelData)
        println("PixelData.getClass.getName: " + PixelData.getClass.getName)

        al.get(TagFromName.PixelData).removeValues
        val voxels = voxList.map(f => f.toShort).toArray

        println("min short: " + voxels.min + "    max short: " + voxels.max)
        println("voxels.size: " + voxels.size)

        al.get(TagFromName.PixelData).setValues(voxels)
        al.write(new FileOutputStream(fileOf(i, "dcm")), TransferSyntax.ImplicitVRLittleEndian, true, true)
    }

    def makeDicom49(i: Int, voxList: Seq[Int], numImages: Int) = {
        val minImagePositionPatientX = -103.85
        val minImagePositionPatientY = -150.141
        val minImagePositionPatientZ = -103.733
        val maxImagePositionPatientZ = 44.5581
        val minSliceLocation = -112.1495819
        val TemporalPositionIdentifier = i + 1
        val NumberOfTemporalPositions = numImages

        val xScale = 0.31
        val yScale = 0.765
        val zScale = 6.4473

        val sliceScale = 6.4474487

        val al = getDicomTemplate

        al.get(TagFromName.SOPInstanceUID).removeValues
        al.get(TagFromName.SOPInstanceUID).addValue(UMROGUID.getUID)

        val zLoc = minImagePositionPatientZ + (zScale * i)

        val ipp = al.get(TagFromName.ImagePositionPatient)
        ipp.removeValues
        ipp.addValue(minImagePositionPatientX + (xScale * i))
        ipp.addValue(minImagePositionPatientY + (yScale * i))
        ipp.addValue(zLoc)

        al.get(TagFromName.Columns).removeValues
        al.get(TagFromName.Columns).addValue(wide)

        al.get(TagFromName.Rows).removeValues
        al.get(TagFromName.Rows).addValue(high)

        al.get(TagFromName.InstanceNumber).removeValues
        al.get(TagFromName.InstanceNumber).addValue(i + 1)

        al.get(TagFromName.SliceLocation).removeValues
        al.get(TagFromName.SliceLocation).addValue(minSliceLocation + (sliceScale * i))

        val voxels = voxList.map(f => f.toShort).toArray
        println("min short: " + voxels.min + "    max short: " + voxels.max)
        println("voxels.size: " + voxels.size)

        if (true) {
            val PixelData = al.get(TagFromName.PixelData)
            println("PixelData.getClass.getName: " + PixelData.getClass.getName)
            al.get(TagFromName.PixelData).removeValues
            al.get(TagFromName.PixelData).setValues(voxels)
        }
        else {
            //val PixelData = AttributeFactory.newAttribute(TagFromName.PixelData)
            val PixelData = new OtherWordAttribute(TagFromName.PixelData)
            // println("PixelData.getClass.getName: " + PixelData.getClass.getName)
            al.remove(TagFromName.PixelData)
            def s2b(s: Short): Array[Byte] = {
                val out = Array.ofDim[Byte](2)
                out(0) = ((s >> 8) & 0xff).toByte
                out(1) = (s & 0xff).toByte
                out
            }
            val bytes: Array[Byte] = voxels.map(s => s2b(s)).flatten
            PixelData.setValues(bytes)
            al.put(PixelData)
        }

        al.write(new FileOutputStream(fileOf(i, "dcm")), TransferSyntax.ExplicitVRLittleEndian, true, true)
    }

    private def removeOld = inDir.listFiles.map(f => if (f.getName.startsWith(filePrefix)) f.delete)

    def main(args: Array[String]): Unit = {
        val start = System.currentTimeMillis

        if (false) {
            patient = "31"
            wide = 256
            high = 256
            val data = readFile
            removeOld

            val floatList = RawByte.bytesToFloat(RawByte.swapBytes4(data, Seq(3, 2, 1, 0)))

            println("floatList.size: " + floatList.size)
            val flds = floatList.distinct.sorted
            println("floatList.distinct.sorted.size: " + flds.size)
            printSeq(flds)

            val maxPixVal = (2.5).toFloat
            val pixList = scaleToRange(floatList.map(f => if (f > maxPixVal) maxPixVal else f), 0, 0xff)

            val voxList = scaleToRange(floatList, 0, 0xffff)

            println("voxList distinct sorted size: " + voxList.distinct.sorted.size)
            println("voxList distinct sorted: ")
            printSeq(voxList.distinct.sorted)

            val numImages = data.size / (wh * 4)
            println("data.size: " + data.size)
            println("numImages: " + numImages)
            (0 until numImages).map(i => saveImage(getImg(i, pixList), fileOf(i, "png")))

            (0 until numImages).map(i => makeDicom31(i, getImg(i, voxList), numImages))
        }

        if (true) {
            patient = "49"
            wide = 512
            high = 512

            val data = readFile
            removeOld

            val floatList = RawByte.bytesToFloat(RawByte.swapBytes4(data, Seq(3, 2, 1, 0)))

            println("floatList.size: " + floatList.size)
            val flds = floatList.distinct.sorted
            println("floatList.distinct.sorted.size: " + flds.size)
            printSeq(flds)

            val maxPixVal = (2.5).toFloat
            val pixList = scaleToRange(floatList.map(f => if (f > maxPixVal) maxPixVal else f), 0, 0xff)

            val voxList = scaleToRange(floatList, 0, 0xffff)

            println("voxList distinct sorted size: " + voxList.distinct.sorted.size)
            println("voxList distinct sorted: ")
            printSeq(voxList.distinct.sorted)

            val numImages = data.size / (wh * 4)
            println("data.size: " + data.size)
            println("numImages: " + numImages)
            (0 until numImages).map(i => saveImage(getImg(i, pixList), fileOf(i, "png")))

            (0 until numImages).map(i => makeDicom49(i, getImg(i, voxList), numImages))
        }

        println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
        System.exit(0)
    }

}