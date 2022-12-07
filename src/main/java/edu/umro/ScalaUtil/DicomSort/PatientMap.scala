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
