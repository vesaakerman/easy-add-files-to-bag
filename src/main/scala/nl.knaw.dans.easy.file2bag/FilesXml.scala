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

import java.nio.file.Path

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import nl.knaw.dans.lib.string._ // required by maven for isBlank, ignored by Intellij when using java-11

import scala.util.Try
import scala.xml.transform.{ RewriteRule, RuleTransformer }
import scala.xml.{ Elem, Node }

object FilesXml extends DebugEnhancedLogging {

  def apply(oldFilesXml: Elem, accessRights: String, destinationPath: Path, mimeType: String): Try[Node] = {

    // easy-deposit-api does not supply rights at all
    // so here we don't supply a default
    // https://github.com/DANS-KNAW/easy-deposit-api/blob/eef71618e8b776fb274da123f8011510499c741a/src/main/scala/nl.knaw.dans.easy.deposit/docs/FilesXml.scala#L40-L42
    val itemContent = if (accessRights.isBlank) Seq[Node]()
                      else <accessibleToRights>{ accessRights }</accessibleToRights>
    val newItem =
        <file filepath={ "data/" + destinationPath }>
          <dcterms:format>{ mimeType }</dcterms:format>
          { itemContent }
        </file>
    logger.info(newItem.toOneLiner)

    object insertElement extends RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case Elem(boundPrefix, "files", _, boundScope, children @ _*) =>
          <files>
            { children }
            { newItem }
          </files>.copy(prefix = boundPrefix, scope = boundScope)
        case other => other
      }
    }
    Try {
      new RuleTransformer(insertElement).transform(oldFilesXml).head
    }
  }
}
