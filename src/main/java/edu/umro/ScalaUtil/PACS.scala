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

import scala.xml.Node

/**
 * Define a DICOM device.
 */
class PACS(val aeTitle: String, val host: String, val port: Int) {

  /**
   * Construct from a string of the format aetitle:host:port
   */
  def this(composite: String) = this(composite.split(":")(0), composite.split(":")(1), composite.split(":")(2).toInt)

  /**
   * Construct from XML of the form
   *
   *     <Ignored AETitle='myAETitle' Host='141.214.125.209' Port='15678' />
   */
  def this(node: Node) = this(PACS.getAttr(node, "AETitle"), PACS.getAttr(node, "Host"), PACS.getAttr(node, "Port").toInt)

  override def toString = aeTitle + " = " + host + ":" + port
}

object PACS {
  def getAttr(node: Node, name: String) = (node \ ("@" + name)).text.toString
}