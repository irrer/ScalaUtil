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

    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String): Unit = {
        Log.get.info("Received from " + callingAETitle + " file " + " DICOM file " + fileName)
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
    def cmove(specification: AttributeList, srcPacs: PACS, dstPacs: PACS): Option[String] = {
        if (subDirFile.isDefined) {
            subDirFile.get.mkdirs
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

    /** Start a DICOM receiver. */
    private def startReceiver: Unit = {
        val dispatcherThread = new Thread(dispatcher)
        dispatcherThread.start();
        Log.get.info("Started DICOM receiver: " + myPacs)
    }

    startReceiver
}

object DicomReceiver {

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

        val mainDir = new File("""D:\tmp\archive_migration_from_xstor_to_velocity\""")

        val cpXstorPacs = new PACS("CP_XSTOR_IRRER", "141.214.125.209", 15656)
        val irrerPacs = new PACS("IRRER", "141.214.125.209", 15678)
        val remotePacs = new PACS("UMRO_ARCHIVE", "10.30.3.69", 104)
        val localPacs = cpXstorPacs

        val dicomReceiver = new DicomReceiver(mainDir, localPacs)

        //val seriesUID = "1.3.6.1.4.1.22361.48658618118952.539916499.1500572921197.3"
        val seriesUID = "1.2.840.113704.1.111.7924.1428068730.3"

        val id = addAttr(TagFromName.SeriesInstanceUID, seriesUID, buildIdentifier)

        println("localPacs: " + localPacs)
        println("remotePacs: " + remotePacs)

        dicomReceiver.setSubDir("test_cmove")
        println("Putting files into " + dicomReceiver.getSubDir.getAbsolutePath)
        Utility.deleteFileTree(dicomReceiver.getSubDir)
        val success = dicomReceiver.cmove(id, remotePacs, localPacs)

        success match {
            case Some(msg) => println("Failed: " + msg)
            case _ => println("success")
        }

        System.exit(0)
    }

}
