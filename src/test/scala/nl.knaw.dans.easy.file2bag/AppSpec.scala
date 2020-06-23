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
import java.util.UUID

import better.files.StringExtensions
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.file2bag.fixture.FileSystemSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{ Failure, Success }

class AppSpec extends AnyFlatSpec with Matchers with FileSystemSupport {
  private val uuid = UUID.randomUUID()

  "addFiles" should "add payload to file system and files.xml" in {
    val bag = createBagWithEmptyFilesXml
    val oldSize = (bag / "metadata" / "files.xml").size

    new EasyAddFilesToBagApp(null).addFiles(
      bag.baseDir / "..",
      ((testDir / "twister-files").createDirectories() / "some.txt").writeText("") / "..",
      (testDir / "input.csv").writeText("p,a,_,r,id\nsome.txt,YES,,ANONYMOUS,easy-dataset:16"),
      (testDir / "f2v-output.csv").writeText(s"id,uuid\neasy-dataset:16,$uuid"),
      (testDir / "log.csv").path,
    ) shouldBe Success(s"1 records written to $testDir/log.csv")

    (testDir / "log.csv").contentAsString shouldBe
      s"""path,rights,fedoraId,comment
         |some.txt,ANONYMOUS,easy-dataset:16,saved at $testDir/bags/$uuid/data/original/some.txt
         |""".stripMargin
    (bag.data/ "original" / "some.txt") should exist
    (bag.data/ "original").children should have size(1)
    (bag / "metadata" / "files.xml").size shouldBe >(oldSize) // exact content tested with FilesXmlSpec
  }

  it should "fail on missing UUID" in {
    new EasyAddFilesToBagApp(null).addFiles(
      testDir / "bag",
      testDir / "twister-files",
      testDir / "input.csv",
      (testDir / "f2v-output.csv").writeText(s"id,uuid\neasy-dataset:16"),
      (testDir / "log.csv").path,
    ) should matchPattern {
      case Failure(e: Throwable) if e.getMessage == "too short record in datasets file: CSVRecord [comment='null', recordNumber=2, values=[easy-dataset:16]]" =>
    }
  }

  it should "fail on invalid UUID" in {
    new EasyAddFilesToBagApp(null).addFiles(
      testDir / "bag",
      testDir / "twister-files",
      testDir / "input.csv",
      (testDir / "f2v-output.csv").writeText(s"id,uuid\neasy-dataset:16,blabla"),
      (testDir / "log.csv").path,
    ) should matchPattern {
      case Failure(e: Throwable) if e.getMessage == "Invalid uuid. CSVRecord [comment='null', recordNumber=2, values=[easy-dataset:16, blabla]]" =>
    }
  }

  it should "report skipped/failed input lines" in {
    val input =
      """path,archive,_,rights,fedoraId
        |whoops.txt,Y,,,easy-dataset:17
        |some.txt,Yes,,,easy-dataset:16
        |some.txt,YES,,,easy-dataset:15
        |huh.txt,YES,,,easy-dataset:15
        |another.txt,No,,,easy-dataset:16
        |none.txt,No,,,
        |null.txt,No
        |blabla.txt,YES,,SOME,easy-dataset:16
        |""".stripMargin
    val uuid2 = UUID.randomUUID()
    val datasets =
      s"""id,uuid,ignored
         |easy-dataset:16,$uuid,blabla
         |easy-dataset:15,$uuid2
         |""".stripMargin
    new EasyAddFilesToBagApp(null).addFiles(
      createBagWithEmptyFilesXml.baseDir / "..",
      ((testDir / "twister-files").createDirectories() / "some.txt").writeText("") / "..",
      (testDir / "input.csv").writeText(input),
      (testDir / "f2v-output.csv").writeText(datasets),
      (testDir / "log.csv").path,
    ) shouldBe Success(s"8 records written to $testDir/log.csv")

    (testDir / "log.csv").contentAsString shouldBe
      s"""path,rights,fedoraId,comment
         |whoops.txt,,easy-dataset:17,FAILED: no bag-id found
         |some.txt,,easy-dataset:16,saved at $testDir/bags/$uuid/data/original/some.txt
         |some.txt,,easy-dataset:15,FAILED: java.nio.file.NoSuchFileException: $testDir/bags/$uuid2/bagit.txt
         |huh.txt,,easy-dataset:15,FAILED: java.io.FileNotFoundException: $testDir/twister-files/huh.txt (No such file or directory)
         |another.txt,,easy-dataset:16,SKIPPED (archive=NO)
         |none.txt,,,SKIPPED no fedora-id
         |"",,,"SKIPPED: to few fields in CSVRecord [comment='null', recordNumber=8, values=[null.txt, No]]"
         |blabla.txt,SOME,easy-dataset:16,"FAILED: java.lang.Exception: SOME is not one of NONE, ANONYMOUS, KNOWN, RESTRICTED_REQUEST, RESTRICTED_GROUP, "
         |""".stripMargin
  }

  private def createBagWithEmptyFilesXml = {
    val content = """<files xmlns:dcterms="http://purl.org/dc/terms/"/>"""
    for {
      bag <- DansV0Bag.empty(testDir / "bags" / uuid.toString)
      _ <- bag.addTagFile(content.inputStream, Paths.get("metadata/files.xml"))
      _ <- bag.save()
    } yield bag
  }.getOrElse(fail("could not create test bag"))
}
