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

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import java.io.File
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.TransferSyntax
import java.io.FileOutputStream
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.SequenceAttribute

// Special for Tong: Mon Nov 16 14:11:21 EST 2015

object TongEdit {

    val DEFAULT_TRANSFER_SYNTAX = TransferSyntax.ImplicitVRLittleEndian;

    /*
    private val baseDir = new File("""M:\For_Jim\Philips_MRI_Sub27\601.T1W_3D_TFE_SENSE\Tract_to_T1""")
    private val newBaseDir = new File("""M:\For_Jim\Philips_MRI_Sub27\601.T1W_3D_TFE_SENSE\Tract_to_T1_New""")

    private val baseDir = new File("""M:\For_Jim\GE_MRI_Sub11\2.AXIAL_3D__T1\Tract_to_T1""")
    private val newBaseDir = new File("""M:\For_Jim\GE_MRI_Sub11\2.AXIAL_3D__T1\Tract_to_T1_New""")

    private val baseDir = new File("""M:\For_Jim\Siemens_MRI_Sub57\2.t1_mprage_sag_p2_iso_0.9_siemens\Tract_to_T1""")
    private val newBaseDir = new File("""M:\For_Jim\Siemens_MRI_Sub57\2.t1_mprage_sag_p2_iso_0.9_siemens\Tract_to_T1_New""")

    private val baseDir = new File("""M:\For_Jim\Sub 46\Tract_to_T1""")
    private val newBaseDir = new File("""M:\For_Jim\Sub 46\Tract_to_T1_New""")

    private val baseDir = new File("""M:\For_Jim\Sub 48""")
    private val newBaseDir = new File("""M:\For_Jim\Sub 48_New""")
    
    private val baseDir = new File("""M:\For_Jim\Sub 50""")
    private val newBaseDir = new File("""M:\For_Jim\Sub 50_New""")
    
    private val baseDir = new File("""M:\For_Jim\Sub 54""")
    private val newBaseDir = new File("""M:\For_Jim\Sub 54_New""")
    
    private val baseDir = new File("""M:\For_Jim\Sub 38""")
    private val newBaseDir = new File("""M:\For_Jim\Sub 38_New""")
    
    private val baseDir = new File("""W:\For_Jim\extra\S""")
    private val newBaseDir = new File("""W:\For_Jim\extra\S_New""")
    
    private val baseDir = new File("""W:\For_Jim\extra\W""")
    private val newBaseDir = new File("""W:\For_Jim\extra\W_New""")
    
    private val baseDir = new File("""M:\LowGrade_3T\MRIpreTreatmentPlanning\51_HEKMATI_ALI\20121019_PreRT\Tract_to_T1""")
    private val newBaseDir = new File("""M:\LowGrade_3T\MRIpreTreatmentPlanning\51_HEKMATI_ALI\20121019_PreRT\Tract_to_T1_New""")

    private val baseDir = new File("""M:\LowGrade_3T\MRIpreTreatmentPlanning\56_DONNER_TESS\20130509_preRT\Tract_to_T1""")
    private val newBaseDir = new File("""M:\LowGrade_3T\MRIpreTreatmentPlanning\56_DONNER_TESS\20130509_preRT\Tract_to_T1_New""")

    private val baseDir = new File("""M:\LowGrade_3T\MRIpreTreatmentPlanning\68_GUSTAFSON_ERIK\20140429_preRT\Tract_to_T1""")
    private val newBaseDir = new File("""M:\LowGrade_3T\MRIpreTreatmentPlanning\68_GUSTAFSON_ERIK\20140429_preRT\Tract_to_T1_New""")        
    */

    private val baseDir = new File("""M:\LowGrade_3T\MRIpreTreatmentPlanning\68_GUSTAFSON_ERIK\20140429_preRT\Tract_to_T1""")
    private val newBaseDir = new File("""M:\LowGrade_3T\MRIpreTreatmentPlanning\68_GUSTAFSON_ERIK\20140429_preRT\Tract_to_T1_New""")

    private def makeUidAttr(tag: AttributeTag): Attribute = {
        val attr = AttributeFactory.newAttribute(tag)
        attr.addValue(UMROGUID.getUID)
        attr
    }

    private def makeMRISOPClass(tag: AttributeTag): Attribute = {
        val attr = AttributeFactory.newAttribute(tag)
        attr.addValue(SOPClass.MRImageStorage)
        attr
    }

    private def destFile(srcFile: File, attributeList: AttributeList): File = {
        val newParentDir = new File(newBaseDir, srcFile.getParentFile.getName)
        newParentDir.mkdirs
        val attr = attributeList.get(TagFromName.SOPInstanceUID)
        val newName = attributeList.get(TagFromName.SOPInstanceUID).getSingleStringValueOrDefault(srcFile.getName) + ".dcm"
        val newFile = new File(newParentDir, newName)
        newFile
    }

    def replaceOne(al: AttributeList, attr: Attribute) = if (al.get(attr.getTag) != null) al.put(attr)

    val ReferencedSOPClassUID = makeMRISOPClass(TagFromName.ReferencedSOPClassUID)
    val MediaStorageSOPClassUID = makeMRISOPClass(TagFromName.MediaStorageSOPClassUID)
    val SOPClassUID = makeMRISOPClass(TagFromName.SOPClassUID)

    private def replaceRecursively(attributeList: AttributeList, replacementList: List[Attribute]): Unit = {

        /*
        def replaceReferencedSOPClassUID(al: AttributeList): Unit = {
            val a = al.get(ReferencedSOPClassUID.getTag)
            if ((a != null) && a.getSingleStringValueOrEmptyString.equals(SOPClass.EnhancedMRImageStorage)) al.put(ReferencedSOPClassUID)
        }
        */
        attributeList.keySet.toArray.toList.map(tag => {
            val t = tag
            val obj = tag.asInstanceOf[Object]
            val a = attributeList.get(t).asInstanceOf[Attribute]
            if (a.isInstanceOf[SequenceAttribute]) {
                val seq = a.asInstanceOf[SequenceAttribute]
                (0 until seq.getNumberOfItems).map(i => replaceRecursively(seq.getItem(i).getAttributeList, replacementList))
            }
        })
    }

    private def readDicom(dcmFile: File): Option[AttributeList] = {
        val attributeList = new AttributeList
        if (dcmFile.getName.toLowerCase.endsWith(".dcm")) {
            try {
                attributeList.read(dcmFile)
                if (attributeList.get(TagFromName.SOPInstanceUID) != null)
                    Some(attributeList)
                else {
                    None
                }
            }
            catch {
                case e: Throwable =>
                    None
            }
        }
        else None
    }

    private def fixDicom(dcmFile: File, replacementList: List[Attribute], SeriesInstanceUID: Attribute): Unit = {
        val al = readDicom(dcmFile)
        if (al.isDefined) {
            val attributeList = al.get
            attributeList.put(SeriesInstanceUID)
            replacementList.map(attr => replaceOne(attributeList, attr))
            replaceRecursively(attributeList, replacementList)

            val transferSyntax = attributeList.get(TagFromName.TransferSyntaxUID).getSingleStringValueOrDefault(TransferSyntax.ExplicitVRLittleEndian)
            val newFile = destFile(dcmFile, attributeList)
            //attributeList.write(new FileOutputStream(newFile), TransferSyntax.ImplicitVRLittleEndian, true, true)
            //attributeList.write(new FileOutputStream(newFile), TransferSyntax.ExplicitVRLittleEndian, true, true)
            attributeList.write(new FileOutputStream(newFile), transferSyntax, true, true)
            println("    Created file " + newFile.getAbsolutePath)
        }
        else println("----Ignored file " + dcmFile.getAbsolutePath)
    }

    //val FrameOfReferenceUID = makeUidAttr(TagFromName.FrameOfReferenceUID)
    //val StudyInstanceUID = makeUidAttr(TagFromName.StudyInstanceUID)

    private def fixDir(dir: File): Unit = {
        val SeriesInstanceUID = makeUidAttr(TagFromName.SeriesInstanceUID)

        println("Starting directory " + dir.getAbsolutePath)

        val replacements = List(
            //ReferencedSOPClassUID,
            //MediaStorageSOPClassUID,
            SOPClassUID)

        val dicomList = dir.listFiles.map(dcmFile => fixDicom(dcmFile, replacements, SeriesInstanceUID))
        println("Finished directory " + dir.getAbsolutePath)
    }

    def main(args: Array[String]): Unit = {
        val start = System.currentTimeMillis
        val subDirList = baseDir.listFiles.filter(f => f.isDirectory).map(d => fixDir(d))
        println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
        System.exit(0)
    }

}