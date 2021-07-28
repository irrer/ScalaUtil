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

package test.java;

import java.io.File;
import com.pixelmed.dicom.Attribute;
import com.pixelmed.dicom.AttributeFactory;
import com.pixelmed.dicom.AttributeList;
import com.pixelmed.dicom.TagFromName;
import com.pixelmed.dicom.TransferSyntax;

/**
 * @author irrer
 *
 */

object SortBySeries {
  val start = System.currentTimeMillis

  //private val inDirName = """T:\Radonc_Shared\shared\Physics\Users\irrer\MRCT2\100180016"""
  private val inDirName = """D:\tmp\aqa\Phase2\foo"""

  private val inDir = new File(inDirName)

  private val outDir = new File(inDir, "out")

  private def processFile(file: File, index: Int, size: Int): Unit = {
    val al = new AttributeList()
    al.read(file)

    val sd = al.get(TagFromName.SeriesDescription)
    val seriesDescRaw = if (sd == null) "" else sd.getSingleStringValueOrEmptyString
    val seriesDesc = seriesDescRaw.replace('-', '_').replaceAll("[^a-zA-Z0-9\\.]", "_")
    val seriesSop = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrDefault("unknown")
    val seriesDir = new File(outDir, seriesSop + "_" + seriesDesc)
    seriesDir.mkdirs

    val sopUid = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
    val fileName = sopUid + ".dcm"

    val outFile = new File(seriesDir, fileName)

    val bytes = edu.umro.util.Utility.readBinFile(file)
    edu.umro.util.Utility.writeFile(outFile, bytes)
    val elapsed = System.currentTimeMillis - start
    val pct = ((index + 1) * 100.0) / size
    println(pct.formatted("%7.3f pct") + " " + (index + 1).formatted("Count: %6d") + " " + elapsed.formatted("  ms: %11d") + " wrote file: " + outFile)
  }

  def main(args: Array[String]): Unit = {

    val fileList = inDir.listFiles.filter(f => f.isFile && f.getName.toLowerCase.endsWith(".dcm"))
    println("number of input files: " + fileList.size)

    outDir.mkdirs
    fileList.zipWithIndex.map(fi => processFile(fi._1, fi._2, fileList.size))
    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
  }

}
