package test.java

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import java.io.File
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.TransferSyntax
import java.io.FileOutputStream

// Special for Yue: Mon Nov 16 14:11:21 EST 2015

object YueEdit {

    val DEFAULT_TRANSFER_SYNTAX = TransferSyntax.ImplicitVRLittleEndian;

    private val outDir = new File("TriggerTime")

    private def destFile(srcFile: File, attributeList: AttributeList): File = {
        val newFile = new File(outDir, srcFile.getName)
        println("Source: " + srcFile.getAbsolutePath + "    Dest: " + newFile.getAbsolutePath)
        newFile
    }

    def replaceOne(al: AttributeList, attr: Attribute) = if (al.get(attr.getTag) != null) al.put(attr)

    private def readDicom(dcmFile: File): Option[AttributeList] = {
        val attributeList = new AttributeList
        try {
            attributeList.read(dcmFile)
            if (attributeList.get(TagFromName.SOPInstanceUID) != null)
                Some(attributeList)
            else {
                println("Ignoring file.  Not DICOM: " + dcmFile.getAbsolutePath)
                None
            }
        }
        catch {
            case e: Throwable =>
                println("Ignoring file.  Error interpreting as DICOM: " + dcmFile.getAbsolutePath + " error: " + e.getMessage)
                None
        }
    }

    private def writeDicom(attributeList: AttributeList, newFile: File): Unit = {
        val transferSyntax = attributeList.get(TagFromName.TransferSyntaxUID).getSingleStringValueOrDefault(DEFAULT_TRANSFER_SYNTAX)
        attributeList.write(new FileOutputStream(newFile), transferSyntax, true, true)
    }

    /**
     * Create output directory and remove old output files.
     */
    private def init: Unit = {
        def del(file: File): Unit = {
            file.delete
            if (file.exists) throw new RuntimeException("Could not delete file " + file.getAbsolutePath)
        }
        outDir.mkdirs
        if (!outDir.isDirectory) throw new RuntimeException("Could not create output directory " + outDir.getAbsolutePath)

        val fileList = outDir.listFiles
        if (fileList != null) fileList.map(f => del(f))
    }

    private def makeTriggerTime(offset: Int, volumeSize: Int, InstanceNumber: Int, RepetitionTime: Int): Attribute = {
        val attr = AttributeFactory.newAttribute(TagFromName.TriggerTime)
        val tt = offset + (((InstanceNumber + (volumeSize-1)) / volumeSize) * RepetitionTime)
        attr.addValue(tt)
        attr
    }

    private def modifyAttributeList(offset: Int, volumeSize: Int, attributeList: AttributeList): Unit = {
        val InstanceNumber = attributeList.get(TagFromName.InstanceNumber).getSingleIntegerValueOrDefault(-1)
        if (InstanceNumber < 1) throw new RuntimeException("Bad InstanceNumber: " + InstanceNumber)
        val RepetitionTime = attributeList.get(TagFromName.RepetitionTime).getSingleIntegerValueOrDefault(-1)
        if (RepetitionTime < 1) throw new RuntimeException("Bad RepetitionTime: " + RepetitionTime)

        val TriggerTime = makeTriggerTime(offset, volumeSize, InstanceNumber, RepetitionTime)
        attributeList.put(TriggerTime)

    }

    private def addTT(offset: Int, volumeSize: Int, dcmFile: File): Unit = {
        val alOpt = readDicom(dcmFile)

        if (alOpt.isDefined) {
            val attributeList = alOpt.get
            modifyAttributeList(offset, volumeSize, attributeList)

            val newFile = destFile(dcmFile, attributeList)
            writeDicom(attributeList, newFile)
        }
    }

    private def usage: Unit = {
        println("Usage: addtrigtime -o [offset] -s [slices per volume] dicom_file1 dicom_file2 ...")
        System.exit(1)
    }

    def getOffset(args: Array[String]): Int = {
        if (!args(0).equalsIgnoreCase("-o")) usage
        args(1).toInt
    }

    def getVolumeSize(args: Array[String]): Int = {
        if (!args(2).equalsIgnoreCase("-s")) usage
        args(3).toInt
    }

    def main(args: Array[String]): Unit = {
        val start = System.currentTimeMillis
        try {
            if (args.size < 5) usage
            init
            val offset = getOffset(args)
            val volumeSize = getVolumeSize(args)
            val fileList = args.drop(4)
            
            println("Using offset: " + offset + "    volumeSize: " + volumeSize + "     Number of files: " + fileList.size)
            fileList.map(n => new File(n)).filter(f => f.isFile).sorted.map(d => addTT(offset, volumeSize, d))
        }
        catch {
            case t: Throwable => {
                println("Failure: " + t.getMessage)
                usage
            }
        }
        println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
        System.exit(0)
    }

}