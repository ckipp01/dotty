package dotty.tools.scaladoc
package util

import org.junit.Test
import org.junit.Assert.*

class JSONTest:
  @Test
  def testStrings =
    assertEquals(quoteStr("""ala"""), jsonString("""ala"""))
    assertEquals(quoteStr("""\""""), jsonString("""""""))
