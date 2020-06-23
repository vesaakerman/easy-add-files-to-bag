/**
 * Copyright (C) 2020 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.file2bag

import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class FilesXmlSpec extends AnyFlatSpec with Matchers {

  private val oldFilesXml =
    <files xmlns:dcterms="http://purl.org/dc/terms/"
           xmlns="http://easy.dans.knaw.nl/schemas/bag/metadata/files/"
           xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           xsi:schemaLocation="http://purl.org/dc/terms/ http://dublincore.org/schemas/xmls/qdc/2008/02/11/dcterms.xsd http://easy.dans.knaw.nl/schemas/bag/metadata/files/ http://easy.dans.knaw.nl/schemas/bag/metadata/files/files.xsd">
        <file filepath="data/path/to/file.txt">
            <dcterms:format>text/plain</dcterms:format>
            <accessibleToRights>NONE</accessibleToRights>
            <visibleToRights>RESTRICTED_REQUEST</visibleToRights>
        </file>
        <file filepath="data/quicksort.hs">
            <dcterms:format>text/plain</dcterms:format>
        </file>
    </files>

  "apply" should "add an item to files.xml" in {
    val triedNode = FilesXml(
      oldFilesXml,
      accessRights = "SOME", // garbage is validated in the App class
      mimeType = "text/plain",
      destinationPath = Paths.get("blabla.txt"),
    )
    triedNode shouldBe a[Success[_]]
    val newFileItems = triedNode.get \ "file"
    newFileItems.theSeq.size shouldBe 1 + (oldFilesXml \ "file").theSeq.size
    newFileItems.last.serialize shouldBe
      """<?xml version='1.0' encoding='UTF-8'?>
        |<file filepath="data/blabla.txt">
        |  <dcterms:format>text/plain</dcterms:format>
        |  <accessibleToRights>SOME</accessibleToRights>
        |</file>""".stripMargin
  }

  it should "not provide default rights" in {
    val triedNode = FilesXml(
      oldFilesXml,
      accessRights = "",
      mimeType = "text/plain",
      destinationPath = Paths.get("blabla.txt"),
    )
    triedNode shouldBe a[Success[_]]
    (triedNode.get \ "file").last.serialize shouldBe
      """<?xml version='1.0' encoding='UTF-8'?>
        |<file filepath="data/blabla.txt">
        |  <dcterms:format>text/plain</dcterms:format>
        |</file>""".stripMargin
  }
}
