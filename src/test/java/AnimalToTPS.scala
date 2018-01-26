package test.java

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import java.io.File
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.TransferSyntax
import java.io.FileOutputStream
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy
import com.pixelmed.dicom.AttributeTag
import edu.umro.util.UMROGUID

// Special for Yue: Mon Nov 16 14:11:21 EST 2015

object AnimalToTPS {

    private val DEFAULT_TRANSFER_SYNTAX = TransferSyntax.ExplicitVRLittleEndian

    private def readDicomFile(file: File): Option[AttributeList] = {
        var al = new AttributeList
        class DicomClientReadStrategy extends ReadTerminationStrategy {
            override def terminate(attrList: AttributeList, tag: AttributeTag, bytesRead: Long) = {
                al = attrList
                false
            }
        }

        try {
            println("reading file as DICOM: " + file.getAbsolutePath)
            al.read(file, new DicomClientReadStrategy)
        }
        catch {
            case t: Throwable => ;
        }

        if (al.size > 3) Some(al) else {
            println("Non-DICOM file ignored: " + file.getAbsolutePath)
            None
        }
    }

    private def writeDicom(attributeList: AttributeList, newFile: File): Unit = {
        val tx = attributeList.get(TagFromName.TransferSyntaxUID)
        val transferSyntax = {
            if (tx == null) DEFAULT_TRANSFER_SYNTAX
            else tx.getSingleStringValueOrDefault(DEFAULT_TRANSFER_SYNTAX)
        }
        attributeList.write(new FileOutputStream(newFile), transferSyntax, true, true)
    }

    private def putSopInstance(al: AttributeList) = {
        val MediaStorageSOPInstanceUID = al.get(TagFromName.MediaStorageSOPInstanceUID)
        if (MediaStorageSOPInstanceUID == null)
            throw new RuntimeException("no MediaStorageSOPInstanceUID")
        val uid = MediaStorageSOPInstanceUID.getSingleStringValueOrNull
        val sop = AttributeFactory.newAttribute(TagFromName.SOPInstanceUID)
        sop.addValue(uid)
        al.put(sop)
    }

    private def zPosition(al: AttributeList) = al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)

    private def convertDirectory(inDir: File) = {

        val outDir = new File(inDir.getParent, inDir.getName + "_TPS")
        outDir.mkdirs
        if (!outDir.isDirectory) {
            throw new RuntimeException("Could not make output directory:\n    " + outDir.getAbsolutePath + "\n   This is probably a write permission or disk full problem.")
        }
        outDir.listFiles.map(f => f.delete)

        val extraConstantAttrValues: List[(AttributeTag, String)] = List(
            (TagFromName.StudyInstanceUID, UMROGUID.getUID),
            (TagFromName.SeriesInstanceUID, UMROGUID.getUID),
            (TagFromName.FrameOfReferenceUID, UMROGUID.getUID),
            (TagFromName.PatientBirthDate, "18000124"),
            (TagFromName.PatientSex, "O"),
            (TagFromName.AccessionNumber, "none"),
            (TagFromName.StudyID, "none"),
            (TagFromName.ReferringPhysicianName, "none"),
            (TagFromName.StudyDate, "18000124"),
            (TagFromName.StudyTime, "000000"),
            (TagFromName.PositionReferenceIndicator, ""),
            (TagFromName.Modality, "CT"),
            (TagFromName.Manufacturer, "none"),
            (TagFromName.ManufacturerModelName, "none"),
            (TagFromName.KVP, "0"),
            (TagFromName.SamplesPerPixel, "1"))

        val extraConstAttr = extraConstantAttrValues.map(tv => {
            val attr = AttributeFactory.newAttribute(tv._1)
            attr.addValue(tv._2)
            attr
        })

        val imageTypeAttr = {
            val attr = AttributeFactory.newAttribute(TagFromName.ImageType)
            attr.addValue("ORIGINAL")
            attr.addValue("PRIMARY")
            attr.addValue("AXIAL")
            attr
        }

        val commonAttr = extraConstAttr :+ imageTypeAttr

        def putInstanceNumber(al: AttributeList, index: Int) = {
            val attr = AttributeFactory.newAttribute(TagFromName.InstanceNumber)
            attr.addValue(index)
            al.put(attr)
        }

        def doFile(al: AttributeList, index: Int, sliceThickness: Double) = {
            commonAttr.map(a => al.put(a))
            putSopInstance(al)
            putInstanceNumber(al, index)
            val outFile = new File(outDir, index.formatted("%05d.dcm"))
            writeDicom(al, outFile)
        }

        val alList = inDir.listFiles.toList.map(f => readDicomFile(f)).flatten.sortWith((a, b) => zPosition(a) < zPosition(b))

        val sliceThickness = {
            if (alList.isEmpty) 0
            else {
                (zPosition(alList.last) - zPosition(alList.head)) / (alList.size - 1)
            }
        }

        alList.zipWithIndex.map(ali => doFile(ali._1, ali._2, sliceThickness))
    }

    def main(args: Array[String]): Unit = {
        //        val fileList = inDir.listFiles.toList.zipWithIndex.map(fi => doFile(fi._1, fi._2))
        //
        //        println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
        convertDirectory(new File("""D:\downloads\GE 3T MRI\primitive"""))
        System.exit(0)
    }

}