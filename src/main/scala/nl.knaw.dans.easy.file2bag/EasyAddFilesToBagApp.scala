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

import java.nio.charset.Charset.defaultCharset
import java.nio.file.{ Path, Paths }
import java.util.UUID

import better.files.{ File, StringExtensions }
import nl.knaw.dans.bag.v0.DansV0Bag
import nl.knaw.dans.easy.file2bag.Command.FeedBackMessage
import nl.knaw.dans.easy.file2bag.EasyAddFilesToBagApp._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVPrinter, CSVRecord }
import org.apache.tika.Tika
import resource.managed

import scala.collection.JavaConverters._
import scala.util.{ Failure, Success, Try }
import scala.xml.XML

class EasyAddFilesToBagApp(configuration: Configuration) extends DebugEnhancedLogging {

  def addFiles(bags: File,
               files: File,
               metadataCSV: File,
               datasetsCSV: File,
               csvLogFile: Path,
              ): Try[FeedBackMessage] = {

    def addPayloadWithRights(input: MetadataRecord): LogRecord = {
      val bagDir = bags / input.bagId.toString
      val payloadSource = files / input.path.toString
      val payloadDestination = Paths.get("original").resolve(input.path)
      val filesXmlPath = Paths.get("metadata/files.xml")
      val filesXmlFile = (bagDir / filesXmlPath.toString).toString()
      val triedString = for {
        _ <- validate(input.rights)
        format <- Try(tika.detect(payloadSource.toJava))
        _ = logger.info(input.toString)
        bag <- DansV0Bag.read(bagDir)
        oldFilesXml <- Try(XML.loadFile(filesXmlFile))
        _ <- bag.addPayloadFile(payloadSource, payloadDestination)
        newFilesXml <- FilesXml(oldFilesXml, input.rights, payloadDestination, format)
        _ <- bag.removeTagFile(filesXmlPath)
        _ <- bag.addTagFile(newFilesXml.serialize.inputStream, filesXmlPath)
        _ <- bag.save
      } yield s"saved at $bagDir/data/$payloadDestination"
      val comment = triedString.toEither.fold(s"FAILED: " + _, identity)
      LogRecord(input.path, input.rights, input.fedoraId, comment)
    }

    def execute(datasets: Map[String, UUID], printer: CSVPrinter)
               (inputCsvRecord: CSVRecord): Try[Unit] = {
      logger.info(inputCsvRecord.toString)
      MetadataRecord(datasets, inputCsvRecord)
        .fold(identity, addPayloadWithRights)
        .print(printer)
    }

    LogRecord.disposablePrinter(csvLogFile).apply { printer =>
      for {
        datasets <- parse(datasetsCSV, fedoraToUuid)
        rows <- parse(metadataCSV, execute(datasets.toMap, printer))
      } yield s"${ rows.size } records written to ${ csvLogFile.toAbsolutePath }"
    }
  }
}

object EasyAddFilesToBagApp {
  private val tika = new Tika

  //https://github.com/DANS-KNAW/easy-schema/blob/5879174909ae68c07a6ac683ffb5435f17894a94/lib/src/main/resources/bag/metadata/files/2017/09/files.xsd#L110-L133
  private val allowed = Seq("NONE", "ANONYMOUS", "KNOWN", "RESTRICTED_REQUEST", "RESTRICTED_GROUP", "")

  private def validate(rights: String): Try[Unit] = {
    allowed.find(_ == rights)
      .map(_ => Success(()))
      .getOrElse(Failure(new Exception(s"$rights is not one of ${ allowed.mkString(", ") }")))
  }

  private def fedoraToUuid(record: CSVRecord): (String, UUID) = {
    if (record.size() < 2)
      throw new IllegalArgumentException(s"too short record in datasets file: $record")
    Try(UUID.fromString(record.get(1)))
      .map(uuid => record.get(0) -> uuid)
      .getOrElse(throw new IllegalArgumentException(s"Invalid uuid. $record"))
  }

  def parse[T](file: File, extract: CSVRecord => T): Try[Iterable[T]] = {
    managed(CSVParser.parse(file.toJava, defaultCharset(), CSVFormat.RFC4180))
      .map(parseCsv(_).map(extract))
      .tried
  }

  private def parseCsv(parser: CSVParser): Iterable[CSVRecord] = {
    parser.asScala.filter(_.asScala.nonEmpty).drop(1)
  }
}
