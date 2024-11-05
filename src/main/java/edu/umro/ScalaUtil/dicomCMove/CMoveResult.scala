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

import java.io.File

/**
  * Result returned by a C-MOVE.
  *
  * @param errorMessage None indicates success, the presence of a message indicates failure.
  * @param dir Directory where DICOM files were put.  This will be created whether the C-MOVE succeeds or
  *            fails.  It is the responsibility of the caller to clean this up.
  */
case class CMoveResult(errorMessage: Option[String], dir: File) {}
