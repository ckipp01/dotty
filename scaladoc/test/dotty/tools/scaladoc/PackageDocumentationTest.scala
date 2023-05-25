package dotty.tools.scaladoc

import org.junit.Assert.*
import com.vladsch.flexmark.util.{ast as mdu, sequence}
import com.vladsch.flexmark.{ast as mda}
import scala.jdk.CollectionConverters.*

class PackageDocumentationTest extends ScaladocTest("packageobjdocs"):
  override def runTest: Unit = withModule { module =>
    module.members.values
      .find {
        case member if member.kind == Kind.Package => true
        case _                                     => false
      }
      .flatMap(_.docs)
      .map(_.body)
      .fold(
        throw AssertionError("No package found or documentation is not present")
      ) {
        case node: mdu.ContentNode =>
          val text = node
            .getDescendants()
            .asScala
            .toList
            .map {
              case node: mdu.ContentNode => node.getContentChars().toString()
              case _                     => ""
            }
            .mkString
          assertTrue(
            "Documentation for package is incorrect",
            text.contains("It's a test")
          )
        case _ =>
          throw AssertionError("No documentation node found in package docs")
      }
  }
end PackageDocumentationTest
