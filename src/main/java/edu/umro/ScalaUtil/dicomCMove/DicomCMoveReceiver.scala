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

package edu.umro.ScalaUtil.dicomCMove

import com.pixelmed.dicom.StoredFilePathStrategy
import com.pixelmed.network.StorageSOPClassSCPDispatcher
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PACS

import java.io.Closeable
import java.io.File

/**
  * Support for receiving incoming DICOM files and storing them in files.
  */
case class DicomCMoveReceiver(mainDir: File, thisPacs: PACS) extends Logging with Closeable {

  private val receivedObjectHandler = new DicomCMoveReceivedObjectHandler

  /** Name of subdirectory to put incoming DICOM files in. */
  private var subDirFile: Option[File] = None

  /**
    * Getter for current subdirectory.
    * @return current subdirectory.
    */
  def getSubDir: File = subDirFile.get

  /**
    * Set the target directory; where the files will be put.  The caller is responsible for making
    * sure the directory is empty before and also deleting the directory after the C-MOVE.
    * If the new directory does not exist then it is created.
    *
    * An exception will be thrown if there is a problem creating it.
    *
    * @param subDir Directory to receive files. Must be a directory under the main directory.
    */
  def setSubDir(subDir: File): Unit = {
    val sub = subDir.getAbsolutePath
    val main = mainDir.getAbsolutePath
    if (sub.startsWith(main) && (sub.length > main.length))
      this.subDirFile = Some(subDir)
    else
      throw new RuntimeException("Must specify a proper sub-directory of the main directory " + main + "     specified: " + sub)
    subDir.mkdirs()
  }

  /**
    * Use the strategy of storing all files in a single directory.  This makes it simple for
    * the caller to maintain the received files.
    */
  private class StoredFilePathStrategyJobFolders extends StoredFilePathStrategy {
    override def makeStoredFilePath(sopInstanceUID: String): String = {
      val file = new File(getSubDir, sopInstanceUID + ".dcm")
      val path = file.getAbsolutePath.substring(mainDir.getAbsolutePath.length + 1)
      path
    }
  }

  /**
    * Service provider which runs in a thread, acting like a PACS by receiving DICOM files.
    */
  private def SCPDispatcher: StorageSOPClassSCPDispatcher = {
    logger.info(s"Starting DICOM dispatcher with PACS $thisPacs")
    new StorageSOPClassSCPDispatcher(
      thisPacs.port, // port that we are listening on
      thisPacs.aeTitle, // our AETitle
      mainDir, // directory for temporary and fetched files
      new StoredFilePathStrategyJobFolders, // strategy for naming incoming DICOM files
      receivedObjectHandler
    )
  }

  private val scpDispatcher = SCPDispatcher

  /**
    * Thread that is processing incoming DICOM files.  Keeping this around makes termination (see close) possible.
    */
  private val dispatcherThread = new Thread(scpDispatcher)

  /**
    * Start a DICOM receiver in its own thread.  After the thread has started, The method
    * waits for one second before returning so that there is a greater assurance that the
    * receiver is ready to actually receive files.
    */
  private def startReceiver(): Unit = {
    logger.info(s"Starting DICOM C-MOVE Receiver to receive incoming DICOM files: $thisPacs")
    dispatcherThread.start()

    val waitTime_sec = 10

    val timeout = System.currentTimeMillis() + waitTime_sec * 1000
    while ((!scpDispatcher.isReady) && (System.currentTimeMillis() < timeout)) {
      logger.info("Waiting for SCPDispatcher for DICOM receiver to start ...")
      Thread.sleep(200) // wait for thread to start.
    }
    Thread.sleep(200) // make sure it is ready
    if (scpDispatcher.isReady) {
      logger.info("SCPDispatcher for DICOM receiver is ready.")
    } else {
      logger.warn(s"Waited $waitTime_sec seconds for SCPDispatcher for DICOM receiver to be ready, but it still indicates it is not ready.  Proceeding anyway.")
    }
  }

  startReceiver()

  /**
    * Shut down the receiving thread.
    */
  override def close(): Unit = {
    try {
      logger.info(s"Shutting down DICOM receiver for $thisPacs")
      scpDispatcher.shutdown()
      Thread.sleep(1000)
      logger.info(s"Shutting down DICOM receiver thread for $thisPacs")
      dispatcherThread.interrupt()
      Thread.sleep(1000)
      val status = if (dispatcherThread.isAlive) "failed" else "succeeded"
      logger.info(s"Shutdown of receiver for $thisPacs : $status")
    } catch {
      case t: Throwable =>
        logger.warn(s"Exception while shutting down receiver for $thisPacs : ${fmtEx(t)}")
    }
  }
}
