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

package edu.umro.ScalaUtil

import com.pixelmed.dicom.AttributeList
import edu.umro.util.Utility
import resource.managed

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.util.zip.ZipOutputStream
import scala.annotation.tailrec

object FileUtil {

  /**
    * Copy the input stream to the output until the input is empty.  Note that the output is not closed.
    *
    * @return The number of bytes copied.
    */
  def copyStream(input: InputStream, output: OutputStream): Long = {
    val len = 8 * 1024
    val buffer = new Array[Byte](len)
    val EOF = -1
    @tailrec
    def copy(size: Long): Long = {
      val n = input.read(buffer)
      if (n == EOF) {
        size
      } else {
        output.write(buffer, 0, n)
        copy(size + n)
      }
    }
    copy(0)
  }

  /**
    * Recursively compare two directories or files and throw an exception if their content is different.
    *
    * @param a
    *            One file or directory.
    *
    * @param b
    *            The other file or directory.
    * @return
    * @throws FileNotFoundException On file not found.
    * @throws IOException On permission and other errors.
    */
  def compareDirs(a: File, b: File): Unit = {
    if (a.isDirectory && b.isDirectory) {
      val aList = a.listFiles
      val bList = b.listFiles
      if (aList.length != bList.length) throw new RuntimeException("Missing file")
      aList.foreach(aa => compareDirs(aa, new File(b, aa.getName)))
    } else {
      if (!(a.isFile && b.isFile && edu.umro.util.Utility.compareFiles(a, b))) throw new RuntimeException("files not equal")
    }
  }

  /**
    * Recursively compare two directories or files and throw an exception if
    * their content is different.  Any file access errors will result in a return of 'false';
    *
    * @param a
    *            One file or directory.
    *
    * @param b
    *            The other file or directory.
    *
    * @return True if the same, false if different.
    *
    * @throws FileNotFoundException On file not found.
    * @throws IOException On permission and other errors.
    */
  def compareFolders(a: File, b: File): Boolean = {
    try {
      compareDirs(a, b)
      true
    } catch {
      case _: Throwable => false
    }
  }

  /**
    * Replace all characters in a name that are not supported by the Windows file system.
    *
    * @param name: File name
    *
    * @param replacement: Character to be used instead of original.
    *
    * This will work for *nix systems too.  It will replace some characters that would be
    * allowed in *nix, but using them is generally not a good idea anyway.
    */
  def replaceInvalidFileNameCharacters(name: String, replacement: Char): String = {
    val modified = (0 until name.length)
      .map(i => {
        val c = name.charAt(i)
        c match {
          case _ if (c == '\\') || (c == '/') || (c == ':') || (c == '*') || (c == '"') || (c == '<') || (c == '>') => replacement
          case _ if (c > 31) && (c < 127)                                                                           => c
          case _                                                                                                    => replacement
        }
      })
      .toArray
    new String(modified)
  }

  /**
    * Given a file or directory, make a byte array out of the zipped contents.  If it is a directory, include children.
    *
    * @param inFileList Zip this list of files.  If a file is a directory, then zip the whole tree below it.
    *
    * @param excludePatternList: Exclude any file whose name matches this pattern.  If the file is a directory, then its children will be excluded too.
    *
    * @param excludeFileList: Exclude any file equal on this list .  If the file is a directory, then its children will be excluded too.
    *
    * @param outputStream: Write to this stream.
    */
  def readFileTreeToZipStream(inFileList: Seq[File], excludePatternList: Seq[String], excludeFileList: Seq[File], outputStream: OutputStream): Unit = {
    def includeAble(file: File) = {
      val name = file.getName
      !excludePatternList.exists(e => name.matches(e)) && !excludeFileList.exists(e => e.equals(file))
    }

    def addOneFileToZip(file: File, zipOut: ZipOutputStream, parentName: Option[String]): Unit = {
      if (includeAble(file)) {
        val entryName = if (parentName.isDefined) parentName.get + "/" + file.getName else file.getName
        if (file.isDirectory) file.listFiles.map(f => addOneFileToZip(f, zipOut, Some(entryName)))
        else {
          val data = Utility.readBinFile(file)
          val zipEntry = new ZipEntry(entryName)
          zipOut.putNextEntry(zipEntry)
          zipOut.write(data)
          zipOut.closeEntry()
        }
      }
    }

    managed(new ZipOutputStream(outputStream)) acquireAndGet { zipOut =>
      {
        inFileList.map(inFile => addOneFileToZip(inFile, zipOut, None))
      }
    }
  }

  def readFileTreeToZipByteArray(inFileList: Seq[File], excludePatternList: Seq[String] = Seq[String](), excludeFileList: Seq[File] = Seq[File]()): Array[Byte] = {
    managed(new ByteArrayOutputStream) acquireAndGet { baOs =>
      {
        readFileTreeToZipStream(inFileList, excludePatternList, excludeFileList, baOs)
        baOs.toByteArray
      }
    }
  }

  /**
    * Given a file tree, zip it up and write it to a file.
    *
    */
  def readFileTreeToZipFile(inFileList: Seq[File], excludePatternList: Seq[String], excludeFileList: Seq[File], zipFile: File): Unit = {
    zipFile.delete
    managed(new FileOutputStream(zipFile)) acquireAndGet { fos =>
      {
        readFileTreeToZipStream(inFileList, excludePatternList, excludeFileList, fos)
        fos.close()
      }
    }
  }

  /**
    * Given an input stream that defines zipped content, write it as a sequence of named
    * byte arrays.  Directories in the input stream are not listed in the returned list.
    *
    * @param inputStream: Input stream.
    *
    * @return list of (name,content) tuples.
    *
    */
  def writeZipToNamedByteArrays(inputStream: InputStream): Seq[(String, Array[Byte])] = {

    val out = managed(new ZipInputStream(inputStream)) acquireAndGet { zipIn =>
      {
        @tailrec
        def next(list: Seq[(String, Array[Byte])]): Seq[(String, Array[Byte])] = {
          val entry = zipIn.getNextEntry
          if (entry != null) {
            if (!entry.isDirectory) {
              val bos = new ByteArrayOutputStream
              copyStream(zipIn, bos)
              bos.close()
              val result = (entry.getName, bos.toByteArray)
              next(list :+ result)
            } else
              next(list)
          } else list
        }
        // Start processing
        next(Seq[(String, Array[Byte])]())
      }
    }
    out
  }

  /**
    * Given an input stream, write it as a zipped tree of files.
    *
    * @param inputStream: Input stream.
    *
    * @param parentDir: Put new files under this directory.
    *
    */
  def writeZipToFileTree(inputStream: InputStream, parentDir: File): Unit = {

    parentDir.mkdirs

    managed(new ZipInputStream(inputStream)) acquireAndGet { zipIn =>
      {
        @tailrec
        def next(): Unit = {
          val entry = zipIn.getNextEntry
          if (entry != null) {
            val file = new File(parentDir, entry.getName.replace("/", File.separator))
            if (entry.isDirectory) file.mkdirs
            else {
              file.getParentFile.mkdirs
              val fos = new FileOutputStream(file)
              copyStream(zipIn, fos)
              fos.close()
            }
            next()
          }
        }
        // Start processing
        next()
      }
    }
  }

  /**
    * Given a byte array containing zipped files, expand it and write to files.
    *
    * @param byteArray: Input bytes.
    *
    * @param parentDir: Put new files under this directory.
    */
  def writeByteArrayZipToFileTree(byteArray: Array[Byte], parentDir: File): Unit = {
    managed(new ByteArrayInputStream(byteArray)) acquireAndGet { byteArrayIn =>
      {
        writeZipToFileTree(byteArrayIn, parentDir)
      }
    }
  }

  /**
    * Given a zip file, expand it and write to files.
    *
    * @param zipFile: Input zip file.
    *
    * @param parentDir: Put new files under this directory.
    */
  def writeZippedFileToFileTree(zipFile: File, parentDir: File): Unit = {
    managed(new FileInputStream(zipFile)) acquireAndGet { zipFileIn =>
      {
        writeZipToFileTree(zipFileIn, parentDir)
      }
    }
  }

  /**
    * Support writing content to a zipped byte array.
    */
  class ToZipOutputStream() {
    private val streamOut = new ByteArrayOutputStream()
    private val zipOut = new ZipOutputStream(streamOut)

    /**
      * Write bytes to a zipped byte array.
      * @param data Data to write.
      * @param name Path name in zip file, e.g.: foo/bar.txt
      */
    def write(data: Array[Byte], name: String): Unit = {
      val zipEntry = new ZipEntry(name)
      zipOut.putNextEntry(zipEntry)
      zipOut.write(data)
      zipOut.closeEntry()
    }

    /**
      * Write DICOM to a zipped byte array.
      *
      * @param al DICOM to write.
      * @param path Path name in zip file, e.g.: foo/bar.txt
      * @param sourceApplication Name of software application. (arbitrary name).
      */
    def writeDicom(al: AttributeList, path: String, sourceApplication: String): Unit = {
      val outStream = new ByteArrayOutputStream()
      DicomUtil.writeAttributeList(al, outStream, sourceApplication)
      write(outStream.toByteArray, path)
    }

    def finish(): Array[Byte] = {
      zipOut.close()
      streamOut.toByteArray
    }
  }

  /**
    * List the files in a directory sorted alphabetically.  If the directory is empty or
    * there are errors then return an empty list.
    */
  def listFiles(dir: File): Seq[File] = {
    try {
      val list = dir.listFiles.sortBy(_.getName)
      list.toSeq
    } catch {
      case _: Throwable => Seq[File]()
    }
  }

  /**
    * Global lock for synchronizing all file writes so that only one write is being done at
    *  a time (as opposed to being done in parallel).
    */
  private val fileSystemWriteSync = "syncFileUtilWritesToDisk"

  def writeBinaryFile(file: File, data: Array[Byte]): Unit =
    fileSystemWriteSync.synchronized({
      file.delete
      val fos = new FileOutputStream(file)
      fos.write(data)
      fos.flush()
      fos.close()
    })

  def writeFile(file: File, text: String): Unit = writeBinaryFile(file, text.getBytes)

  def readBinaryFile(file: File): Either[Throwable, Array[Byte]] = {
    try {
      val fis = new FileInputStream(file)
      val buf = new Array[Byte](file.length.toInt)
      fis.read(buf)
      fis.close()
      Right(buf)
    } catch {
      case t: Throwable => Left(t)
    }
  }

  def readTextFile(file: File): Either[Throwable, String] = {
    val result = readBinaryFile(file)
    if (result.isLeft) Left(result.left.get)
    else Right(new String(result.right.get))
  }

  /**
    * Append the given data to the given file.  Return None on success.
    */
  def appendFile(file: File, data: Array[Byte]): Option[Throwable] = {
    try {
      val outputStream = new FileOutputStream(file, true)
      outputStream.write(data)
      outputStream.flush()
      outputStream.close()
      None
    } catch {
      case t: Throwable => Some(t)
    }
  }

  /**
    * Get the suffix of the given file name.
    *
    * If the file name ends in '.' or has no '.' then an empty string is returned.
    *
    */
  def getFileSuffix(fileName: String): String = {
    if (fileName.contains('.'))
      FileUtil.replaceInvalidFileNameCharacters(fileName, '_').replaceAll(".*\\.", "")
    else ""
  }

  def getFileSuffix(file: File): String = getFileSuffix(file.getName)

  /**
    * Recursively delete all of the files in a directory tree. The directory
    * itself will be deleted.
    *
    * @param file
    *            Top level file or directory whose files will be deleted. If
    *            this is a regular file, then just this file will be deleted.
    *
    * @return True if all files successfully deleted.
    */
  def deleteFileTree(file: File): Boolean = {
    try {
      if (file.isDirectory) file.listFiles.map(child => deleteFileTree(child))
      file.delete
      !file.exists
    } catch {
      case _: Throwable => false
    }
  }

  /**
    * Perform the given operation on every file in the given file tree. Files in
    * each directory are processed in alphabetical order.
    *
    * Note that this function may throw an exception if there is a file permission problem.
    *
    * @param file Top level file.
    * @param process Process to perform.
    */
  def processFileTree(file: File, process: File => Unit): Unit = {
    process(file)
    if (file.isDirectory) {
      file.listFiles.sortBy(_.getName).foreach(f => processFileTree(f, process))
    }
  }

  def main(args: Array[String]): Unit = { // TODO rm
    // add comment to test git
    val j = edu.umro.ScalaUtil.FileUtil.replaceInvalidFileNameCharacters("gleeParity", 'X')
    println("j: " + j)

    def showPath(file: File): Unit =
      println("File path: " + file.getAbsolutePath)

    processFileTree(new File("""D:\tmp\joann"""), showPath)
  }

}
