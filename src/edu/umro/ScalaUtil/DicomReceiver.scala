package edu.umro.ScalaUtil

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.TagFromName
import com.pixelmed.network.ReceivedObjectHandler
import com.pixelmed.dicom.DicomDictionary
import com.pixelmed.dicom.StoredFilePathStrategy
import com.pixelmed.dicom.StoredFilePathStrategySingleFolder
import edu.umro.util.Log
import com.pixelmed.network.StorageSOPClassSCPDispatcher
import com.pixelmed.network.Association
import com.pixelmed.network.AssociationFactory
import com.pixelmed.network.Association
import java.util.LinkedList
import com.pixelmed.network.PresentationContext
import com.pixelmed.dicom.TransferSyntax
import com.pixelmed.dicom.SOPClass
import com.pixelmed.network.CMoveRequestCommandMessage
import com.pixelmed.network.CompositeResponseHandler
import java.io.ByteArrayOutputStream
import com.pixelmed.dicom.DicomOutputStream
import com.pixelmed.network.AReleaseException
import edu.umro.util.Utility
import java.util.Date
import java.text.SimpleDateFormat
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.StoredFilePathStrategy
import com.pixelmed.network.MoveSOPClassSCU
import com.pixelmed.network.DicomNetworkException
import java.util.HashSet

/**
 * Support for copying files from Varian to the local file system.
 */
class DicomReceiver(mainDir: File, myPacs: PACS) extends ReceivedObjectHandler {

    /** Name of sub-directory to put incoming DICOM files in. */
    private var subDir: String = "init"

    private def currentDir: File = new File(mainDir + File.separator + subDir)

    private class StoredFilePathStrategyJobFolders extends StoredFilePathStrategy {
        override def makeStoredFilePath(sopInstanceUID: String): String = {
            val path = subDir + File.separator + sopInstanceUID + ".dcm" // TODO remove
            subDir + File.separator + sopInstanceUID + ".dcm"
        }
    }

    var total = 0 // TODO rm
    val hist = new java.util.HashSet[String]() // TODO rm
    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String): Unit = {
        total = total + 1
        if (hist.contains(fileName)) {
            println("repeating " + fileName)
            System.exit(99)
        }
        hist.add(fileName)
        Log.get.info("Received from " + callingAETitle + " file " + " DICOM file " + fileName + "    total: " + total)
    }

    private def dispatcher = {
        println("Starting dispatcher with PACS " + myPacs)
        new StorageSOPClassSCPDispatcher(
            myPacs.port, // port that we are listening on
            myPacs.aeTitle, // our AETitle
            mainDir, // directory for temporary and fetched files
            new StoredFilePathStrategyJobFolders, // strategy for naming incoming DICOM files
            this) // ReceivedObjectHandler receivedObjectHandler,
    }

    private def getPresentationContext: LinkedList[PresentationContext] = {
        val presentationContextList = new LinkedList[PresentationContext]

        val tslist = new LinkedList[String]
        // tslist.add(TransferSyntax.ExplicitVRLittleEndian) // TODO keep?
        tslist.add(TransferSyntax.ImplicitVRLittleEndian) // TODO keep?
        //        presentationContextList.add(new PresentationContext(1.toByte, SOPClass.PatientRootQueryRetrieveInformationModelFind, tslist));
        presentationContextList.add(new PresentationContext(3.toByte, SOPClass.PatientRootQueryRetrieveInformationModelMove, tslist));

        //        presentationContextList.add(new PresentationContext(1.toByte, SOPClass.StudyRootQueryRetrieveInformationModelFind, tslist));
        //        presentationContextList.add(new PresentationContext(3.toByte, SOPClass.StudyRootQueryRetrieveInformationModelMove, tslist));

        return presentationContextList;
    }

    /**
     * Perform DICOM C-MOVE.
     *
     * @return true on success
     */
    def cmove(dir: String, specification: AttributeList, srcPacs: PACS, dstPacs: PACS): Boolean = {
        subDir = dir
        currentDir.mkdirs
        val specAsString = specification.toString.replace('\0', ' ')

        // If no QueryRetrieveLevel is specified, then fail badly.
        if (specification.get(TagFromName.QueryRetrieveLevel) == null) {
            throw new RuntimeException("No QueryRetrieveLevel specified")
        }

        //val affectedSOPClass = SOPClass.StudyRootQueryRetrieveInformationModelMove
        val affectedSOPClass = SOPClass.PatientRootQueryRetrieveInformationModelMove

        Log.get.info("Starting C-MOVE of files from " + srcPacs.aeTitle + " to " + dstPacs.aeTitle + " with specification of \n" + specAsString);
        try {
            val moveSOPClassSCU = new MoveSOPClassSCU(
                srcPacs.host, // source host
                srcPacs.port, // source port
                srcPacs.aeTitle, // source aeTitle
                myPacs.aeTitle, // callingAETitle aeTitle
                dstPacs.aeTitle, // moveDestination
                affectedSOPClass, //  affectedSOPClass
                specification) // identifier
            true
        }
        catch {
            case e: Exception => {
                Log.get.severe("CMove error: " + Log.fmtEx(e))
                false
            }
        }
    }

    /** Start a DICOM receiver. */
    private def startReceiver: Unit = {
        val dispatcherThread = new Thread(dispatcher)
        dispatcherThread.start();
        Log.get.info("Started DICOM receiver: " + myPacs)
    }

    startReceiver
}

object DicomReceiver {

    def buildDicomSpecifier: AttributeList = {
        val al = new AttributeList
        def addAttr(tag: AttributeTag, value: String): Unit = {
            val a = AttributeFactory.newAttribute(tag)
            a.addValue(value)
            al.put(a)
        }

        addAttr(TagFromName.QueryRetrieveLevel, "IMAGE")

        //addAttr(TagFromName.SOPInstanceUID, "1.2.246.352.71.3.824327626427.2199934.20121221170933")
        //addAttr(TagFromName.PatientID, "$909595")

        //addAttr(TagFromName.Modality, "RTPLAN")
        //addAttr(TagFromName.SOPInstanceUID, "1.2.246.352.71.5.824327626427.215737.20140306160710")
        addAttr(TagFromName.SOPInstanceUID, "1.2.246.352.71.5.824327626427.225957.20140326092327")
        //addAttr(TagFromName.PatientID, "Mobius1_3_1")

        al
    }

    def main(args: Array[String]): Unit = {

        val spec = new AttributeList
        def addAttr(tag: AttributeTag, value: String): Unit = {
            val a = AttributeFactory.newAttribute(tag)
            a.addValue(value)
            spec.put(a)
        }
        addAttr(TagFromName.QueryRetrieveLevel, "SERIES") //  TODO is this right?
        // addAttr(TagFromName.QueryRetrieveLevel, "IMAGE") // TODO WLQA does it this way
        //addAttr(TagFromName.SeriesInstanceUID, "1.3.12.2.1107.5.2.19.45228.201307081513214157314794.0.0.0")   // patient ID 0000024
        addAttr(TagFromName.SeriesInstanceUID, "1.3.6.1.4.1.22361.48658618118952.539916499.1500572921197.3") // patient ID COMPARE_TEST

        //addAttr(TagFromName.PatientID, "COMPARE_TEST")

        val mainDir = new File("""D:\tmp\archive_migration_from_xstor_to_velocity\""")
        val subDir = new File(mainDir, "test_cmove")
        val subDirName = subDir.getName
        println("Putting files into " + subDir.getAbsolutePath)
        Utility.deleteFileTree(subDir)
        subDir.mkdirs

        val cpXstorPacs = new PACS("CP_XSTOR_IRRER", "141.214.125.209", 15656)
        val irrerPacs = new PACS("IRRER", "141.214.125.209", 15678)
        val remotePacs = new PACS("UMRO_ARCHIVE", "10.30.3.69", 104)

        val localPacs = cpXstorPacs

        println("localPacs: " + localPacs)
        println("remotePacs: " + remotePacs)

        val dicomReceiver = new DicomReceiver(mainDir, localPacs)

        val success = dicomReceiver.cmove(subDirName, spec, remotePacs, localPacs)

        println(if (success) "success" else "fail")
    }

}
