package dotty.tools.dotc.parsing

object parseFile extends ParserTest:

  def main(args: Array[String]): Unit =
    if args.isEmpty then
      println("usage: scala test.parseFile file1.scala ... fileN.scala")
    for arg <- args do
      val tree = parse(arg)
      println("parsed: " + arg)
      println(tree.show)
