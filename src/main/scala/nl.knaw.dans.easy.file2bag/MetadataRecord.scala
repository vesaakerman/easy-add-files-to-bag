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

import java.nio.file.{ Path, Paths }
import java.util.UUID

import cats.syntax.either._
import org.apache.commons.csv.CSVRecord

case class MetadataRecord(fedoraId: String, bagId: UUID, path: Path, rights: String)

object MetadataRecord {
  def apply(datasets: Map[String, UUID], input: CSVRecord): Either[LogRecord, MetadataRecord] = {
    if (input.size() < 5)
      LogRecord(Paths.get(""), "", "", s"SKIPPED: to few fields in $input").asLeft
    else if (input.getRecordNumber == 0)
           LogRecord(Paths.get(""), "", "", s"SKIPPED: assumed header $input").asLeft
         else {
           val path = Paths.get(input.get(0))
           val archive = input.get(1).toUpperCase()
           val rights = input.get(3)
           val fedoraId = input.get(4)

           def create = datasets.get(fedoraId).map(uuid =>
             new MetadataRecord(fedoraId, uuid, path, rights).asRight
           ).getOrElse(LogRecord(path, rights, fedoraId, "FAILED: no bag-id found").asLeft)

           (archive, fedoraId) match {
             case (_, "") => LogRecord(path, rights, fedoraId, s"SKIPPED no fedora-id").asLeft
             case ("YES", _) | ("Y", _) => create
             case _ => LogRecord(path, rights, fedoraId, s"SKIPPED (archive=$archive)").asLeft
           }
         }
  }
}
