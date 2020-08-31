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
import scala.xml.{ Elem, Node, XML, NamespaceBinding }

object FilesXml extends DebugEnhancedLogging {

  val DEFAULT_PREFIX = "dcterms"
  val DEFAULT_URI = "http://purl.org/dc/terms/"

  def apply(filesXml: Elem, accessRights: String, destinationPath: Path, mimeType: String): Try[Node] = {

    // easy-deposit-api does not supply rights at all
    // so here we don't supply a default
    // https://github.com/DANS-KNAW/easy-deposit-api/blob/eef71618e8b776fb274da123f8011510499c741a/src/main/scala/nl.knaw.dans.easy.deposit/docs/FilesXml.scala#L40-L42

    val formatTagPrefix = Option(filesXml.scope.getPrefix(DEFAULT_URI)).getOrElse(DEFAULT_PREFIX)
    val filesXmlWithPossiblyAddedNamespace  = Option(filesXml.scope.getURI(formatTagPrefix))
      .map(_ => filesXml)
      .getOrElse(filesXml.copy(scope = NamespaceBinding(formatTagPrefix, DEFAULT_URI, filesXml.scope)))
    val accessibleTo = if (accessRights.isBlank) ""
                       else s"<accessibleToRights>$accessRights</accessibleToRights>"
    val newFileElement =
      XML.loadString(
        s"""<file filepath="data/$destinationPath">
           <$formatTagPrefix:format>$mimeType</$formatTagPrefix:format>
           $accessibleTo
        </file>""")
    logger.info(newFileElement.toOneLiner)

    object insertElement extends RewriteRule {
      override def transform(node: Node): Seq[Node] = node match {
        case Elem(boundPrefix, "files", _, boundScope, children @ _*) =>
          <files>
            { children }
            { newFileElement }
          </files>.copy(prefix = boundPrefix, scope = boundScope)
        case other => other
      }
    }
    Try {
      new RuleTransformer(insertElement).transform(filesXmlWithPossiblyAddedNamespace).head
    }
  }
}
