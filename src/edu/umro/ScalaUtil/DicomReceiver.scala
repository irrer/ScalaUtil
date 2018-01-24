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
class DicomReceiver(mainDir: File, myPacs: PACS, receivedObjectHandler: ReceivedObjectHandler) {

    def this(mainDir: File, myPacs: PACS) = this(mainDir, myPacs, new DicomReceiver.DefaultReceivedObjectHandler)

    lazy val mainDirName = mainDir.getAbsolutePath

    /** Name of sub-directory to put incoming DICOM files in. */
    private var subDirFile: Option[File] = None

    def getSubDir: File = subDirFile.get

    def setSubDir(subDir: File): File = {
        val sub = subDir.getAbsolutePath
        val main = mainDir.getAbsolutePath
        if (sub.startsWith(main) && (sub.size > main.size))
            this.subDirFile = Some(subDir)
        else
            throw new RuntimeException("Must specify a proper sub-directory of the main directory " + main + "     specified: " + sub)
        getSubDir
    }

    def setSubDir(subDirName: String): File = {
        setSubDir(new File(mainDir + File.separator + subDirName))
    }

    private class StoredFilePathStrategyJobFolders extends StoredFilePathStrategy {
        override def makeStoredFilePath(sopInstanceUID: String): String = {
            val file = new File(getSubDir, sopInstanceUID + ".dcm")
            val path = file.getAbsolutePath.substring(mainDirName.size + 1)
            path
        }
    }

    private def dispatcher = {
        println("Starting dispatcher with PACS " + myPacs)
        new StorageSOPClassSCPDispatcher(
            myPacs.port, // port that we are listening on
            myPacs.aeTitle, // our AETitle
            mainDir, // directory for temporary and fetched files
            new StoredFilePathStrategyJobFolders, // strategy for naming incoming DICOM files
            receivedObjectHandler)
    }

    private def getPresentationContext: LinkedList[PresentationContext] = {
        val presentationContextList = new LinkedList[PresentationContext]

        val tslist = new LinkedList[String]
        tslist.add(TransferSyntax.ImplicitVRLittleEndian)
        tslist.add(TransferSyntax.ExplicitVRLittleEndian)
        presentationContextList.add(new PresentationContext(3.toByte, SOPClass.PatientRootQueryRetrieveInformationModelMove, tslist));
        return presentationContextList;
    }

    /**
     * Perform DICOM C-MOVE.
     *
     * @return true on success
     */
    def cmove(specification: AttributeList, srcPacs: PACS, dstPacs: PACS, affectedSOPClass: String): Option[String] = {
        if (subDirFile.isDefined) {
            subDirFile.get.mkdirs
            val specAsString = specification.toString.replace('\0', ' ')

            // If no QueryRetrieveLevel is specified, then fail badly.
            if (specification.get(TagFromName.QueryRetrieveLevel) == null) {
                throw new RuntimeException("No QueryRetrieveLevel specified")
            }

            //val affectedSOPClass = SOPClass.StudyRootQueryRetrieveInformationModelMove
            //val affectedSOPClass = SOPClass.PatientRootQueryRetrieveInformationModelMove

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
                None
            }
            catch {
                case e: Exception => {
                    Some("CMove error: " + Log.fmtEx(e))
                }
            }
        }
        else Some("The subdirectory value must be set before performing a C-MOVE.  Use setSubDir")
    }

    @deprecated ("Use <code>cmove(specification: AttributeList, srcPacs: PACS, dstPacs: PACS, affectedSOPClass: String)</code> instead.")
    def cmove(specification: AttributeList, srcPacs: PACS, dstPacs: PACS): Option[String] = {
        cmove(specification: AttributeList, srcPacs: PACS, dstPacs, SOPClass.PatientRootQueryRetrieveInformationModelMove)
    }

    /** Start a DICOM receiver. */
    private def startReceiver: Unit = {
        println("==================================== DicomReceiver.startReceiver") // TODO rm
        val dispatcherThread = new Thread(dispatcher)
        dispatcherThread.start
        Thread.sleep(500) // wait for receiver to start
        Log.get.info("Started DICOM receiver: " + myPacs)
    }

    startReceiver
}

object DicomReceiver {

    class DefaultReceivedObjectHandler extends ReceivedObjectHandler {
        override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String): Unit = {
            Log.get.info("Received DICOM from " + callingAETitle + " file " + " DICOM file " + fileName)
        }
    }

    def addAttr(tag: AttributeTag, value: String, identifier: AttributeList): AttributeList = {
        val a = AttributeFactory.newAttribute(tag)
        a.addValue(value)
        identifier.put(a)
        identifier
    }

    def buildIdentifier: AttributeList = {
        addAttr(TagFromName.QueryRetrieveLevel, "SERIES", new AttributeList)
    }

    def main(args: Array[String]): Unit = {

        val mainDir = new File("""D:\tmp\wl\no_contours""")

        val cpXstorPacs = new PACS("CP_XSTOR_IRRER", "141.214.125.209", 15656)
        val irrerPacs = new PACS("IRRER", "141.214.125.209", 15678)
        val umroArchivePacs = new PACS("UMRO_ARCHIVE", "10.30.3.69", 104)
        val wlqaTestPacs = new PACS("WLQA_TEST", "141.214.125.209", 5682)
        val vmsdbdPacs = new PACS("VMSDBD", "141.214.124.167", 105)

        val localPacs = wlqaTestPacs
        val remotePacs = vmsdbdPacs

        val dicomReceiver = new DicomReceiver(mainDir, localPacs)

        //val seriesUID = "1.3.6.1.4.1.22361.48658618118952.539916499.1500572921197.3"
        //val seriesUID = "1.2.840.113704.1.111.7924.1428068730.3"
        //val seriesUID = "1.2.246.352.62.2.5632972184956645828.12605259155903877552"

        def attempt(qrLevel: String, moveType: String) = {

            println("moveType: " + moveType)
            val al = new AttributeList

            val critera = List(
                (TagFromName.QueryRetrieveLevel, qrLevel),
                // (TagFromName.SeriesInstanceUID, "1.2.246.352.62.2.5632972184956645828.12605259155903877552") //            ,
                (TagFromName.PatientID, "QASRSWLCBCT2017Q4"),
                (TagFromName.InstanceCreationDate, "20180123"))
            //(TagFromName.SeriesDate, "20171204"))

            //(TagFromName.InstanceCreationDate, "20170522")

            critera.map(c => addAttr(c._1, c._2, al))

            println("localPacs: " + localPacs)
            println("remotePacs: " + remotePacs)

            dicomReceiver.setSubDir("test_cmove")
            println("Putting files into " + dicomReceiver.getSubDir.getAbsolutePath)
            Utility.deleteFileTree(dicomReceiver.getSubDir)
            val success = dicomReceiver.cmove(al, remotePacs, localPacs, moveType)

            success match {
                case Some(msg) => println("Failed: " + msg)
                case _ => println("success")
            }
            println("\n")
        }

        val moveList = List(
            SOPClass.StudyRootQueryRetrieveInformationModelMove)
        //SOPClass.PatientRootQueryRetrieveInformationModelMove,
        //SOPClass.PatientStudyOnlyQueryRetrieveInformationModelMove

        val qrLevelList = List(
            "STUDY",
            "PATIENT",
            "SERIES",
            "IMAGE",
            "SR DOCUMENT")

        for (qr <- qrLevelList; m <- moveList) {
            attempt(qr, m)
        }

        Thread.sleep(1000)
        // explicit exit is important to kill receiver thread
        System.exit(0)
    }

}
