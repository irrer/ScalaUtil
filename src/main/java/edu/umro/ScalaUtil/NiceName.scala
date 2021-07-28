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

import java.io.File

/**
 * Example of a program that reads a DICOM file, adds an attribute, and then writes it.
 *
 * @author irrer
 *
 */

object NiceName {

  private val start = System.currentTimeMillis

  private val regEx = "[^a-zA-Z0-9_\\.\\-]"
  private val goodChar = '_'.toString

  private var total = 0

  private var renamed = 0

  private def niceName(file: File): Unit = {

    total = total + 1
    // change made in WSRO1009 and then by WSRO)0103 // TODO rm
    val oldName = file.getName
    val newName = oldName.replaceAll(regEx, goodChar)
    val newFile: File = if (!oldName.equals(newName)) {
      val nf = new File(file.getParentFile, newName)
      if (file.renameTo(nf)) println("renamed " + file.getAbsolutePath + " -> " + nf.getAbsolutePath)
      renamed = renamed + 1
      nf
    } else file
    if (newFile.isDirectory) newFile.listFiles.map(f => niceName(f))
  }

  def main(args: Array[String]): Unit = {
    val fileList = args.map(a => new File(a))
    fileList.map(file => niceName(file))
    println("Elapsed ms: " + (System.currentTimeMillis - start) + "    Total files: " + total + "    Files renamed: " + renamed)
  }

}
