package edu.umro.ScalaUtil.DicomSort


/**
 * Contain a mutable list of patients, wrapping it so as to isolate the mutability.
 */
object PatientMap {

  /** Map of all patients found: [PatientID, Patient] */
  private val patientList = scala.collection.mutable.Map[String, Patient]()

  def contains(PatientID: String): Boolean = patientList.contains(PatientID)


  /**
   * Put into the map.  Wrap it in a synchronized in case this is ever done in parallel.
   *
   * @param PatientID New patient ID.
   * @param patient   New Patient information.
   */
  def put(PatientID: String, patient: Patient): Unit = {
    patientList.synchronized(patientList.put(PatientID, patient))
  }

  def get(PatientID: String): Patient = patientList(PatientID)

  def size: Int = patientList.size

  def values: Seq[Patient] = patientList.values.toIndexedSeq
}
