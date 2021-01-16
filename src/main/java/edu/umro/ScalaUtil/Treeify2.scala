package edu.umro.ScalaUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

object Treeify2 {

  /**
   * Given a list of FREF (frame of reference UID) indicies, format it as text with a leading _.  Example
   *
   * Seq(1,2,3,4) --> _FREF_1_FREF_2_FREF_3_FREF_4
   *
   * @param frefList List of FREF indicies.
   * @return
   */
  private def frefToString(frefList: Seq[Int]): String = frefList.map(i => "FREF_" + i).mkString("_", "_", "")


  /**
   * Get an attribute as a string.  It is important to create a copy of the string so that the
   * attribute list can be garbage collected.  This keeps the program's memory footprint small,
   * which is important when dealing with zillions of files.
   *
   * @param al  Get from this list
   * @param tag Tag to get
   * @return A copy of the attributes as text with whitespace trimmed.
   */
  def getAttr(al: AttributeList, tag: AttributeTag) = new String(al.get(tag).getSingleStringValueOrEmptyString.trim)


  /**
   * Search for a usable date+time in an attribute list.  First valid one wins, so the order matters.
   */
  private val dateTimeTagPairs = Seq(
    (TagFromName.TreatmentDate, TagFromName.TreatmentTime),
    (TagFromName.ContentDate, TagFromName.ContentTime),
    (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime),
    (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
    (TagFromName.SeriesDate, TagFromName.SeriesTime),
    (TagFromName.StudyDate, TagFromName.StudyTime),
    (TagFromName.RTPlanDate, TagFromName.RTPlanTime),
    (TagFromName.StructureSetDate, TagFromName.StructureSetTime))

  /**
   * Try getting a date that represents when the file was created.
   *
   * @param al Get from this list
   * @return A date that represents when the file was created.
   */
  private def getDateTime(al: AttributeList): Date = {
    val date = dateTimeTagPairs.flatMap(dtp => DicomUtil.getTimeAndDate(al, dtp._1, dtp._2)).head
    date
  }


  private val frefList = scala.collection.mutable.Map[String, Int]()


  /**
   * Get the frames of references as a list of indexes.
   *
   * @param al Contains frames of reference.
   * @return List of indexes.
   */
  private def getFrefList(al: AttributeList): Seq[Int] = {
    val list = DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).map(attr => attr.getSingleStringValueOrEmptyString).distinct

    def addToList(frefUID: String): Int = {
      if (!frefList.contains(frefUID)) {
        val index = frefList.size
        frefList.put(frefUID, index)
      }
      frefList(frefUID)
    }

    list.map(frefUID => addToList(frefUID))
  }

  /**
   * Represent a single DICOM file.
   *
   * @param frefInInst List of frame of reference indexes.
   * @param sourceFile Source file.
   * @param date       Date associated with file.
   */
  private case class DicomFile(frefInInst: Seq[Int], sourceFile: File, date: Date) {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss.SSS")
    val dateText: String = dateFormat.format(date)

    def moveTo(seriesDir: File, Modality: String, index: Int, frefList: Seq[Int]): Unit = {
      val frefText = {
        Modality match {
          case "REG" => frefToString(frefList)
          case "RTPLAN" => frefToString(frefList)
          case _ => ""
        }
      }
      val destFile = new File(seriesDir, Modality + "_" + (index + 1).formatted("%03d") + "_" + dateText + ".dcm")
      sourceFile.renameTo(destFile)
      print(".") // indicates progress to the user
    }
  }


  private case class Series(SeriesInstanceUID: String, Modality: String) {
    private val dicomFileList = scala.collection.mutable.Map[String, DicomFile]()

    /**
     * Add a file by putting the related information into the data structures.
     *
     * @param file File to add
     * @param al   Attribute list reflecting contents of file.
     */
    def add(file: File, al: AttributeList): Unit = {
      val SOPInstanceUID = getAttr(al, TagFromName.SOPInstanceUID)
      if (dicomFileList.contains(SOPInstanceUID)) {
        println("\nFile with duplicate SOPInstanceUID ignored: " + file.getAbsolutePath + "  Previous file: " + dicomFileList(SOPInstanceUID).sourceFile.getAbsolutePath)
      }
      else dicomFileList.put(SOPInstanceUID, new DicomFile(getFrefList(al), file, getDateTime(al)))
    }
  }


  private case class Study(StudyInstanceUID: String) {
    private val seriesList = scala.collection.mutable.Map[String, Series]()

    /**
     * Add a file by putting the related information into the data structures.
     *
     * @param file File to add
     * @param al   Attribute list reflecting contents of file.
     */
    def add(file: File, al: AttributeList): Unit = {
      val SeriesInstanceUID = getAttr(al, TagFromName.SeriesInstanceUID)
      if (!seriesList.contains(SeriesInstanceUID)) seriesList.put(SeriesInstanceUID, new Series(SeriesInstanceUID, getAttr(al, TagFromName.Modality)))
      seriesList(SeriesInstanceUID).add(file, al)
    }
  }

  private case class Patient(PatientID: String) {
    private val studyList = scala.collection.mutable.Map[String, Study]()

    /**
     * Add a file by putting the related information into the data structures.
     *
     * @param file File to add
     * @param al   Attribute list reflecting contents of file.
     */
    def add(file: File, al: AttributeList): Unit = {
      val StudyInstanceUID = getAttr(al, TagFromName.StudyInstanceUID)
      if (!studyList.contains(StudyInstanceUID)) studyList.put(StudyInstanceUID, new Study(StudyInstanceUID))
      studyList(StudyInstanceUID).add(file, al)
    }

    def move() = {
      println("\n" + this) // TODO
    }
  }

  /**
   * Read a DICOM file.  If it contains DICOM, then return the corresponding attribute list.
   *
   * @param dicomFile Try to read this as a DICOM file.
   * @return An attribute list or  nothing on failure.
   */
  private def readFile(dicomFile: File): Option[AttributeList] = {
    try {
      val al = new AttributeList
      al.read(dicomFile)
      print(".") // show read progress to user
      Some(al)
    } catch {
      case t: Throwable =>
        println("Unable to read file as DICOM.  Ignoring: " + dicomFile.getAbsolutePath)
        None
    }
  }


  private val patientList = scala.collection.mutable.Map[String, Patient]()

  /**
   * Add the references of the given file to the data structures.
   *
   * @param file
   */
  private def addFile(file: File): Unit = {
    readFile(file) match {
      case Some(al) =>
        print(".")
        val PatientID = getAttr(al, TagFromName.PatientID)
        if (!patientList.contains(PatientID)) patientList.put(PatientID, new Patient(PatientID))
        patientList(PatientID).add(file, al)
      case _ =>
    }
  }


  /**
   * Crawl the file tree, adding any DICOM files to the data structures.
   *
   * @param file
   */
  private def getFilesInTree(file: File): Unit = {
    if (file.isDirectory)
      file.listFiles.foreach(f => getFilesInTree(f))
    else
      addFile(file)
  }

  def main(args: Array[String]): Unit = {
    try {
      val start = System.currentTimeMillis
      if (args.length != 2) {
        println("Usage: treeify inputdir outputdir\nNo files moved.")
        System.exit(1)
      }
      val inDir = new File(args(0))
      val outDir = new File(args(1))

      getFilesInTree(inDir)

      patientList.values.map(patient => patient.move)

      println("\nDone.  Elapsed ms: " + (System.currentTimeMillis - start))
    } catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }

}
