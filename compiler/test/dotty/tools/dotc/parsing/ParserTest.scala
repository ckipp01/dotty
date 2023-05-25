package dotty.tools
package dotc
package parsing

import dotty.tools.io.*
import util.*
import scala.io.Codec
import Tokens.*, Parsers.*
import ast.untpd.*
import scala.collection.mutable.ListBuffer

class ParserTest extends DottyTest:

  def parse(name: String): Tree = parse(new PlainFile(File(name)))

  var parsed = 0
  val parsedTrees = new ListBuffer[Tree]

  def reset() =
    parsed = 0
    parsedTrees.clear()

  def parse(file: PlainFile): Tree = parseSource(SourceFile(file, Codec.UTF8))

  private def parseSource(source: SourceFile): Tree =
    // println("***** parsing " + source.file)
    val parser = new Parser(source)
    val tree = parser.parse()
    parsed += 1
    parsedTrees += tree
    tree

  def parseDir(path: String): Unit = parseDir(Directory(path))

  def parseDir(dir: Directory): Unit =
    for f <- dir.files do
      if f.name.endsWith(".scala") then parse(new PlainFile(f))
    for d <- dir.dirs do parseDir(d.path)

  def parseText(code: String): Tree = parseSource(
    SourceFile.virtual("<code>", code)
  )
end ParserTest
